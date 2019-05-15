defmodule DatabaseEngine.Utils.EventLogger do
  @moduledoc false
  alias DatabaseEngine.DurableQueue
  alias Utilities.Logging

  defmacro log_event(entity_name, entity_id, state, data \\ %{}) do
    quote do
      #      def log_event(entity_name, entity_id, state, data \\ %{}) do

      module_name = __MODULE__
      {func_name, func_arity} = __ENV__.function
      time = Utilities.now()
      node = Utilities.to_string(:erlang.node())
      my_pid = Utilities.to_string(self())
      q_name = "log_event"

      event_data = %{
        "node" => node,
        "pid" => my_pid,
        "module" => module_name,
        "function" => func_name,
        "function_arity" => func_arity,
        "time" => time,
        "entity" => unquote(entity_name),
        "eid" => unquote(entity_id),
        "state" => unquote(state),
        "additional_data" => Jason.encode!(unquote(data))
      }

      additional_data = unquote(data) || %{}
      index_params = additional_data["__index"] || %{}

      to_druid = event_data |> Map.merge(index_params)
      {:ok, stringified} = Jason.encode(to_druid)
      Logging.debug("q:~p event_data:~p", [q_name, event_data])

      case DurableQueue.enqueue_string(q_name, stringified) do
        :ok ->
          :ok

        e ->
          Logging.error("problem to enqueue:~p", [e])
          Logging.error("QPB. event:~p", [stringified])
          :nok
      end

      #      end
    end
  end

  defmacro log_events(list_of_events) do
    quote do
      events = unquote(list_of_events)

      if length(events) > 0 do
        module_name = __MODULE__
        {func_name, func_arity} = __ENV__.function
        time = Utilities.now()
        node = Utilities.to_string(:erlang.node())
        my_pid = Utilities.to_string(self())
        q_name = "log_event"

        event_data =
          events
          |> Enum.map(fn x ->
            rtn = %{
              "node" => node,
              "pid" => my_pid,
              "module" => module_name,
              "function" => func_name,
              "function_arity" => func_arity,
              "time" => x["time"] || Utilities.now(),
              "entity" => x["entity_name"],
              "eid" => x["entity_id"],
              "state" => x["state"],
              "additional_data" => Jason.encode!(x["additional_data"])
            }

            additional_data = x["additional_data"] || %{}
            index_params = additional_data["__index"] || %{}
            to_druid = rtn |> Map.merge(index_params)
            to_druid
          end)

        to_q = event_data |> Enum.map(fn x -> Jason.encode!(x) end)
        Logging.debug("q:~p event_data:~p", [q_name, event_data])

        {send_status, durations} = DurableQueue.enqueue_string_list(q_name, to_q)

        not_send_indices =
          send_status
          |> Enum.filter(fn {ind, status} ->
            case status do
              :ok -> false
              :nok -> true
            end
          end)
          |> Enum.map(fn {ind, _} -> ind end)

        if(length(not_send_indices) > 0) do
          Logging.error("problem to enqueue", [])
        end

        not_send_indices
        |> Enum.map(fn ind ->
          item = to_q |> Enum.at(ind)
          Logging.error("QPB. event:~p", [item])
        end)

        {send_status, durations}
      end
    end
  end
end
