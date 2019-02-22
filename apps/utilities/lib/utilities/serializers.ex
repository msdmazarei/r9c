defmodule Utilities.Serializers.BinSerializer do
  require Bertex

  def serialize(obj) do
    Bertex.encode(obj)
  end

  def deserialize(bin) do
    Bertex.safe_decode(bin)
  end
end

defmodule Utilities.Serializers.JSONSerializer do
  @moduledoc false

  @doc """
  Hello world.

  ## Examples

      iex> Utilities.Serializers.JSONSerializer.serialize(1)
      {:ok, "1"}

      iex> Utilities.Serializers.JSONSerializer.serialize( %{ a: 1})
      {:ok, "{\"a\":1}"}


  """
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  @spec serialize(term) :: {:ok, String.t()} | {:error, Exception.t()}
  def serialize(object) do
    Logging.debug("called", [])
    Jason.encode(object)
  end

  @spec deserialize(String.t()) :: {:ok, term} | {:error, Exception.t()}
  def deserialize(json_string) do
    Jason.decode(json_string)
  end
end
