defmodule Utilities.Logging do
  @moduledoc false
  require Logger

  defmacro debug(message) do
    quote do
      if __MODULE__ == DatabaseEngine.DurableQueue do
        :ok
      else
        Logger.debug(fn ->
          format_str = "<~p>(~p)[~p]-  " <> unquote(message)
          l = [self(), __MODULE__, __ENV__.function]
          :io_lib.format(format_str, l)
        end)
      end
    end
  end

  defmacro debug(message, gholi) do
    quote do
      if __MODULE__ == DatabaseEngine.DurableQueue do
        :ok
      else
        Logger.debug(fn ->
          format_str = "<~p>(~p)[~p]-  " <> unquote(message)
          l = [self(), __MODULE__, __ENV__.function] ++ unquote(gholi)
          :io_lib.format(format_str, l)
        end)
      end
    end
  end

  defmacro info(message) do
    quote do
      Logger.info(fn ->
        format_str = "<~p>(~p)[~p]" <> unquote(message)
        :io_lib.format(format_str, [self(), __MODULE__, __ENV__.function])
      end)
    end
  end

  defmacro info(message, gholi) do
    quote do
      Logger.info(fn ->
        format_str = "<~p>(~p)[~p]-  " <> unquote(message)
        l = [self(), __MODULE__, __ENV__.function] ++ unquote(gholi)
        :io_lib.format(format_str, l)
      end)
    end
  end

  defmacro warn(message) do
    quote do
      Logger.warn(fn ->
        format_str = "<~p>(~p)[~p]" <> unquote(message)
        :io_lib.format(format_str, [self(), __MODULE__, __ENV__.function])
      end)
    end
  end

  defmacro warn(message, gholi) do
    quote do
      Logger.warn(fn ->
        format_str = "<~p>(~p)[~p]-  " <> unquote(message)
        l = [self(), __MODULE__, __ENV__.function] ++ unquote(gholi)
        :io_lib.format(format_str, l)
      end)
    end
  end

  defmacro error(message) do
    quote do
      Logger.error(fn ->
        format_str = "<~p>(~p)[~p]" <> unquote(message)
        :io_lib.format(format_str, [self(), __MODULE__, __ENV__.function])
      end)
    end
  end

  defmacro error(message, gholi) do
    quote do
      Logger.error(fn ->
        format_str = "<~p>(~p)[~p]-  " <> unquote(message)
        l = [self(), __MODULE__, __ENV__.function] ++ unquote(gholi)
        :io_lib.format(format_str, l)
      end)
    end
  end
end
