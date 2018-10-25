defmodule Utilities do
  @moduledoc """
  Documentation for Utilities.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Utilities.hello()
      :world

  """
  def hello do
    :world
  end

  def randseed do
    :rand.seed(:exs1024s)
  end
end
