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


  def serialize(object) do
    Jason.encode(object)
  end


  def deserialize(json_string) do
    Jason.decode(json_string)
  end

end
