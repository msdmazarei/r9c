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
      q_name = "log_event"

      event_data = %{
        "module" => module_name,
        "function" => func_name,
        "function_arity" => func_arity,
        "time" => time,
        "entity" => unquote(entity_name),
        "eid" => unquote(entity_id),
        "state" => unquote(state),
        "additional_data" => unquote(data)
      }

      Logging.debug("q:~p event_data:~p", [q_name, event_data])

      DurableQueue.enqueue(q_name, event_data)
      #      end
    end
  end
end
