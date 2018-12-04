defmodule GatewayCore.Utils.Throttle do
  @moduledoc false

  require Logger
  require Utilities.Logging
  alias Utilities.Logging
  require DatabaseEngine.Interface.KV
  alias DatabaseEngine.Interface.KV
  @throttle_slice_count 5

  @spec check_is_allowed(String.t(), integer(), integer()) :: boolean()
  def check_is_allowed(throttle_name, how_many_allowed, how_much_time) do
    key = "throttle-#{throttle_name}-#{how_many_allowed}-#{how_much_time}"
    Logging.debug("key: ~p", [key])

    throttle_state =
      case KV.get(key) do
        {_, nil} ->
          s = init_throttle_state(how_many_allowed, how_much_time)
          KV.set(key,s)
          s
        {_, v} -> v
      end

    unix_time_now = Utilities.now()

    Logging.debug("throttle_state:~p", [throttle_state])

    time_offset = unix_time_now - throttle_state.start_time
    now_index = div(time_offset * @throttle_slice_count, how_much_time )
    available_quota = now_index  * throttle_state.each_division_count

    Logging.debug("time offset:~p now_index:~p", [time_offset, now_index])

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
    Logging.debug("Correct Parts:~p sum:~p", [correct_parts, total_items_sum])

    case total_items_sum do
      v when v >= available_quota ->
        Logging.debug("could not send")
        false

      _ ->
        {wc,tt_now, correct_parts} =
          case correct_parts |> length() do
            l when l >= now_index ->
              {throttle_state.window_count+1,throttle_state.start_time,
               correct_parts |> List.update_at(now_index, fn x -> x + 1 end)}

            _ ->
              c =
                for i <- 0..throttle_state.count do
                  Enum.at(correct_parts, i) || 0
                end

              {0,unix_time_now, c}
          end

        throttle_state = Map.merge(throttle_state, %{parts: correct_parts, start_time: tt_now,total_done: throttle_state.total_done+1,window_count: wc})
#        if rem(throttle_state.total_done,101) == 0 do
          Logging.info("thorttle_state.total_done:~p, window_c:~p", [throttle_state.total_done+1,throttle_state.window_count])
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
    check_is_allowed(name,6000,60_000)
  end
  def g(name) do
    for _ <- 0..10000 do
      f(name)
      Process.sleep(1)
    end
  end
end
