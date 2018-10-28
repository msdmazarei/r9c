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


  @spec serialize(term) ::
          {:ok, String.t()} | {:error,  Exception.t()}
  def serialize(object) do
    Jason.encode(object)
  end

  @spec deserialize(string()) :: {:ok, term} | {:error, Exception.t()}
  def deserialize(json_string) do
    Jason.decode(json_string)
  end
end
