defmodule Utilities do
  @moduledoc """
  Documentation for Utilities.
  """
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

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

  def to_struct(kind, attrs) do
    struct = struct(kind)

    Enum.reduce(Map.to_list(struct), struct, fn {k, _}, acc ->
      case Map.fetch(attrs, Atom.to_string(k)) do
        {:ok, v} -> %{acc | k => v}
        :error -> acc
      end
    end)
  end

  @spec callback(any(), String.t(), String.t(), list(any())) :: any()
  def callback(
        data,
        module,
        function,
        arguments \\ []
      ) do
    try do
      module =
        if is_atom(module) == false do
          String.to_atom(module)
        end

      function =
        if is_atom(function) == false do
          String.to_atom(function)
        end

      arguments = arguments ++ [data]
      Kernel.apply(module, function, arguments)
    rescue
      e ->
        Logging.error("problem to call module:~p function:~p args:~p error:~p", [
          module,
          function,
          arguments,
          e
        ])
    end
  end
end
