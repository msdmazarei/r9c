defmodule OnlineChargingSystem.Servers.Diameter.ConnectedClientProcess.ProcessInPacketsLoop do
  @mnesia_packets_key "mnesia_packets_key"
  @master_pid "master_pid"
  @terminate_reason "terminate_reason"
  @processed_packets "processed_packets"
  @droped_packets "droped_packets"
  @requeued_packets "requeued_packets"

  @client_config "client_config"
  @client_address "client_address"
  @client_ip "client_ip"
  @client_port "client_port"
  @pname "pname"
  @output_buffer_count "output_buffer_count"
  @per_q_success_failuar_map "per_q_success_failuar_map"

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require DatabaseEngine.Interface.LKV
  alias DatabaseEngine.Interface.LKV

  def get_queue_name_for_dia_packet(client_conf, diameter_bin_packet, client_address) do
    Logging.debug("Called.")

    case diameter_bin_packet do
      <<_::binary-size(8), application_id::size(32), _::binary>> ->
        application_config = client_conf[application_id] || client_conf["default"]

        if application_config != nil do
          if application_config["queue"] != nil do
            application_config["queue"]
          else
            Logging.warn(
              "no queue definition found in existing config(bad config) for client:~p and application_id:~p",
              [
                client_address,
                application_id
              ]
            )

            nil
          end
        else
          Logging.warn("no application config found for client :~p and application_id:~p", [
            client_address,
            application_id
          ])

          nil
        end

      _ ->
        nil
    end
  end

  def process_in_packets(
        mnesia_packets_key,
        client_config,
        client_address,
        client_ip,
        client_port,
        pname,
        output_buffer_count
      ) do
    input_packets = LKV.get(mnesia_packets_key) || []

    if length(input_packets) > 0 do
      dia_structs_to_enqueue =
        input_packets
        |> Enum.map(fn x ->
          qn = get_queue_name_for_dia_packet(client_config, x, client_address)

          if qn != nil do
            Logging.debug("enqueue packet to q:~p", [qn])

            diameter_struct = %DatabaseEngine.Models.DiameterPacket{
              id: UUID.uuid1(),
              client_address: client_ip,
              client_port: client_port,
              capture_timestamp: Utilities.now(),
              packet_bin: x,
              parsed_packet: Utilities.Parsers.Diameter.parse_from_bin(x),
              options: %{},
              internal_callback: %DatabaseEngine.Models.InternalCallback{
                module_name: OnlineChargingSystem.Servers.Diameter.ConnectedClientProcess,
                function_name: :send_response_back_to_client,
                arguments: [{node(), pname, output_buffer_count}]
              }
            }

            {qn, diameter_struct}
          else
            # packet will be dropped
            nil
          end
        end)

      to_enq = dia_structs_to_enqueue |> Enum.filter(fn x -> x != nil end)

      grouped_by_qname = to_enq |> Enum.group_by(fn {q, _} -> q end, fn {_, v} -> v end)

      # we use async task because may we need multiple qname with arrived packets

      tasks =
        grouped_by_qname
        |> Map.to_list()
        |> Enum.map(fn {qname, packets} ->
          Task.async(fn ->
            Logging.debug("spawned for qname:~p to enque list len:~p", [qname, length(packets)])
            {qname, DatabaseEngine.DurableQueue.enqueue_list(qname, packets)}
          end)
        end)

      Logging.debug("awaiting")
      enqueue_task_result_group_by_qname = tasks |> Enum.map(fn x -> Task.await(x) end)

      Logging.debug("All response from enqueuing arrived.")

      per_q_success_failuar_map =
        enqueue_task_result_group_by_qname
        |> Enum.map(fn {qname, resu} ->
          count = resu |> Enum.map(fn {_, res} -> res == :ok end) |> length
          {qname, %{"ok" => count, "nok" => length(resu) - count}}
        end)
        |> Map.new()

      Logging.debug("success_failuar per q:~p", [per_q_success_failuar_map])

      dropped_cause_of_no_qname = length(dia_structs_to_enqueue) - length(to_enq)

      succ_enq_count =
        per_q_success_failuar_map
        |> Map.to_list()
        |> Enum.map(fn {_, %{"ok" => success}} -> success end)
        |> Enum.sum()

      dropped_count = dropped_cause_of_no_qname + length(to_enq) - succ_enq_count

      # carefully think about re_enq
      re_enq = []

      r =
        :mnesia.transaction(fn ->
          current_input_packets = LKV.get_for_update(mnesia_packets_key) || []
          clen = current_input_packets |> length
          olen = input_packets |> length
          Logging.debug("clen:~p olen:~p",[clen, olen])

          new_input_packets =
            if clen > olen do
              new_elems = current_input_packets |> Enum.slice(olen, clen - olen)
              # make new elements first of list
              new_elems ++ re_enq
            else
              re_enq
            end

          LKV.set(mnesia_packets_key, new_input_packets)
        end)

      case r do
        {:aborted, reason} ->
          {:terminate, reason}

        {:atomic, _} ->
          {succ_enq_count, dropped_count, length(re_enq), per_q_success_failuar_map}
      end
    else
      {0, 0, 0, %{}}
    end
  end

  def loop(
        state = %{
          @mnesia_packets_key => mnesia_packets_key,
          @client_config => client_config,
          @client_address => client_address,
          @client_ip => client_ip,
          @client_port => client_port,
          @pname => pname,
          @output_buffer_count => output_buffer_count,
          @processed_packets => processed_packets,
          @droped_packets => droped_packets,
          @requeued_packets => requeued_packets,
          @per_q_success_failuar_map => per_q_success_failuar_map
        }
      ) do
    {continue, state} =
      receive do
        income_msg ->
          case income_msg do
            {sender, back_reference, :stats} ->
              send(
                sender,
                {back_reference,
                 %{
                   @processed_packets => processed_packets,
                   @droped_packets => droped_packets,
                   @requeued_packets => requeued_packets,
                   @per_q_success_failuar_map => per_q_success_failuar_map
                 }}
              )

              {true, state}

            _ ->
              {true, state}
          end
      after
        0 ->
          case process_in_packets(
                 mnesia_packets_key,
                 client_config,
                 client_address,
                 client_ip,
                 client_port,
                 pname,
                 output_buffer_count
               ) do
            {:terminate, reason} ->
              state = state |> Map.put(@terminate_reason, reason)
              {false, state}

            {pp, dp, rp, per_q_success_failuar_map} when is_number(pp) ->
              state =
                if pp == 0 and dp == 0 and rp == 0 do
                  :timer.sleep(20)
                  state
                else
                  state =
                    case state[@per_q_success_failuar_map] do
                      nil ->
                        state |> Map.put(@per_q_success_failuar_map, per_q_success_failuar_map)

                      v when is_map(v) ->
                        new_v =
                          per_q_success_failuar_map
                          |> Map.to_list()
                          |> Enum.reduce(v, fn {q, %{"ok" => succ, "nok" => fail}}, acc ->
                            new_val =
                              case acc[q] do
                                nil ->
                                  %{"ok" => succ, "nok" => fail}

                                %{"ok" => osucc, "nok" => ofail} ->
                                  %{"ok" => succ + osucc, "nok" => fail + ofail}
                              end

                            acc |> Map.put(q, new_val)
                          end)

                        state |> Map.put(@per_q_success_failuar_map, new_v)
                    end

                  state
                  |> Map.put(@processed_packets, (processed_packets || 0) + pp)
                  |> Map.put(@droped_packets, (droped_packets || 0) + dp)
                  |> Map.put(@requeued_packets, (requeued_packets || 0) + rp)
                end

              {true, state}
          end
      end

    if continue do
      loop(state)
    else
      Process.exit(self(), state[@terminate_reason])
    end
  end
end
