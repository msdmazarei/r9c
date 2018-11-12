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
    all_active_nodes() |> Enum.shuffle() |> hd
  end

  @doc """
      gets already active nodes in cluster.
  """
  @spec all_active_nodes() :: list(Atom.t())
  def all_active_nodes() do
    Node.list() ++ [node()]
  end

  @doc """
  gets unixtime
  """
  def now do
    DateTime.utc_now() |> DateTime.to_unix(:millisecond)
  end
end
