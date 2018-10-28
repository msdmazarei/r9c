defmodule Utilities.Logging do
  @moduledoc false
  require Logger

  defmacro debug(message) do
    quote do
      Logger.debug(fn ->
        format_str = "<~p>(~p)[~p]" <> unquote(message)
        :io_lib.format(format_str, [self(), __MODULE__, __ENV__.function])
      end)
    end
  end

  defmacro debug(message, gholi) do
    quote do
      Logger.debug(fn ->
        format_str = "<~p>(~p)[~p]-  " <> unquote(message)
        l = [self(), __MODULE__, __ENV__.function] ++ unquote(gholi)
        :io_lib.format(format_str, l)
      end)
    end
  end
end
