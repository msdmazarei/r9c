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
  @spec hello() :: :world
  def hello do
    :world
  end

  @doc """
    randomize seed.  You need it everytime you need to generate random.
  """

  @spec randseed() :: any()
  def randseed do
    :rand.seed(:exs1024s)
  end

  @doc """
    Gives you a active node.  It will select one randomly.
    Since we first need to ping the node, it may lead to a bottleneck;
    Later in production we need to improve it by building an active 
    `node in memory` database.
    #TODO
  """
  @spec randnode() :: Atom.t()
  def randnode do
    ## later, this needs to be fixed and cached.
    randseed()
    allnodes |> Enum.shuffle() |> hd
  end

  @doc """
      gets already active nodes in cluster.
  """
  @spec allnodes() :: list(Atom.t())
  def allnodes() do
    Node.list() ++ [node()]
  end

  def to_erl_list(x) do
    if is_binary(x) do
      :binary.bin_to_list(x)
    else
      x
    end
  end

  def erl_list_to_iex_string(x) do
    if is_list(x) do
      :binary.list_to_bin(x)
    else
      x
    end
  end

  def nested_tuple_to_list(list) when is_list(list) do
    list |> Enum.map(&nested_tuple_to_list/1)
  end

  def nested_tuple_to_list(tuple) when is_tuple(tuple) do
    tuple |> Tuple.to_list() |> Enum.map(&nested_tuple_to_list/1)
  end

  def nested_tuple_to_list(x), do: x
end
