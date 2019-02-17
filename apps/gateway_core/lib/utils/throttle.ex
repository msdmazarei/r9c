defmodule GatewayCore.Utils.Throttle do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  require DatabaseEngine.Interface.KV
  alias DatabaseEngine.Interface.KV
  alias :mnesia, as: Mnesia

  @throttle_slice_count 5

  defp get_key(n, c, t) do
    "throttle-#{n}-#{c}-#{t}"
  end

  def multiple_is_allowed(throttle_name, list_how_many_how_much) do
    if KV.get("multiple-#{throttle_name}") == nil do
      list_how_many_how_much
      |> Enum.map(fn {co, ti} ->
        KV.set(get_key(throttle_name, co, ti), init_throttle_state(co, ti))
      end)

      KV.set("multiple-#{throttle_name}", true)
    end

    res =
      case Mnesia.transaction(fn ->
             rtn =
               list_how_many_how_much
               |> Enum.reduce(true, fn {count, time}, r ->
                 r and check_is_allowed(throttle_name, count, time)
               end)

             if rtn == false do
               :mnesia.abort(rtn)
             else
               rtn
             end
           end) do
        {:atomic, true} -> true
        _ -> false
      end

    #    Logging.debug("returns:~p", [res])
    res
  end

  @spec check_is_allowed(String.t(), integer(), integer()) :: boolean()
  def check_is_allowed(throttle_name, how_many_allowed, how_much_time) do
    key = "throttle-#{throttle_name}-#{how_many_allowed}-#{how_much_time}"
    #    Logging.debug("key: ~p", [key])

    throttle_state =
      case KV.get(key) do
        nil ->
          s = init_throttle_state(how_many_allowed, how_much_time)
          KV.set(key, s)
          s

        v ->
          v
      end

    unix_time_now = Utilities.now()

    #    Logging.debug("throttle_state:~p", [throttle_state])

    time_offset = unix_time_now - throttle_state.start_time
    now_index = div(time_offset * @throttle_slice_count, how_much_time)
    available_quota = now_index * throttle_state.each_division_count

    #    Logging.debug("time offset:~p now_index:~p", [time_offset, now_index])

    correct_parts =
      case now_index do
        v when v <= @throttle_slice_count ->
          case throttle_state.parts do
            [] ->
              for _ <- 0..throttle_state.count do
                0
              end

            parts ->
              parts
          end

        _ ->
          Map.merge(throttle_state, %{start_time: unix_time_now})
          []
      end

    total_items_sum = correct_parts |> Enum.sum()
    #    Logging.debug("Correct Parts:~p sum:~p", [correct_parts, total_items_sum])

    case total_items_sum do
      v when v >= available_quota ->
        #        Logging.debug("could not send")
        false

      _ ->
        {wc, tt_now, correct_parts} =
          case correct_parts |> length() do
            l when l >= now_index ->
              {throttle_state.window_count + 1, throttle_state.start_time,
               correct_parts |> List.update_at(now_index, fn x -> x + 1 end)}

            _ ->
              c =
                for i <- 0..throttle_state.count do
                  Enum.at(correct_parts, i) || 0
                end

              {0, unix_time_now, c}
          end

        throttle_state =
          Map.merge(throttle_state, %{
            parts: correct_parts,
            start_time: tt_now,
            total_done: throttle_state.total_done + 1,
            window_count: wc
          })

        #        if rem(throttle_state.total_done,101) == 0 do
        #        Logging.debug("thorttle_state.total_done:~p, window_c:~p", [
        #          throttle_state.total_done + 1,
        #          throttle_state.window_count
        #        ])

        #        end
        KV.set(key, throttle_state)
        true
    end
  end

  def init_throttle_state(how_many_allowed, how_much_time) do
    %{
      each_division_count: how_many_allowed / @throttle_slice_count,
      count: @throttle_slice_count,
      parts: [],
      start_time: Utilities.now(),
      total_done: 0,
      window_count: 0
    }
  end

  def f(name) do
    check_is_allowed(name, 6000, 60_000)
  end

  def g(name) do
    for _ <- 0..10000 do
      f(name)
      Process.sleep(1)
    end
  end

  def counter_process(v) do
    receive do
      :end ->
        Logging.debug("counter receiver messages:~p", [v])

      1 ->
        Logging.info("t:~p,inc", [Utilities.now()])
        counter_process(v + 1)

      _ ->
        counter_process(v)
    end
  end

  def test_1(runtime) do
    pid = Kernel.spawn(fn -> counter_process(0) end)
    test_1(Utilities.now() + runtime, pid)
  end

  def test_1(ent, pid) do
    if Utilities.now() > ent do
      send(pid, :end)
    else
      if multiple_is_allowed("gholi-k", [{3, 10_000}, {9, 60_000}]) do
        send(pid, 1)
      else
        Process.sleep(1000)
      end

      test_1(ent, pid)
    end
  end
end
