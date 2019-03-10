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

    all_active_nodes()
    |> Enum.shuffle()
    |> hd
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
    DateTime.utc_now()
    |> DateTime.to_unix(:millisecond)
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
    list
    |> Enum.map(&nested_tuple_to_list/1)
  end

  def nested_tuple_to_list(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> Enum.map(&nested_tuple_to_list/1)
  end

  def nested_tuple_to_list(map) when is_map(map) do
    Enum.reduce(
      Map.to_list(map),
      %{},
      fn {k, v}, acc ->
        new_v =
          if is_list(v) or is_map(v) or is_tuple(v) do
            nested_tuple_to_list(v)
          else
            v
          end

        Map.put(acc, k, new_v)
      end
    )
  end

  def nested_tuple_to_list(x), do: x

  def to_struct(kind, attrs) do
    struct = struct(kind)

    r= Enum.reduce(
      Map.to_list(struct),
      struct,
      fn {k, _}, acc ->
        if k == :__struct__ do
          acc
        else
          case Map.fetch(attrs, Atom.to_string(k)) do
            {:ok, v} -> %{acc | k => v}
            :error -> acc
          end
        end
      end
    )
    r
  end

  def update_struct(struct, attrs) do
    Enum.reduce(
      Map.to_list(struct),
      struct,
      fn {k, _}, acc ->
        case Map.fetch(attrs, Atom.to_string(k)) do
          {:ok, v} -> %{acc | k => v}
          :error -> acc
        end
      end
    )
  end

  @spec callback(any(), String.t(), String.t(), list(any())) :: any()
  def callback(
        data,
        module,
        function_name,
        arguments
      ) do
    try do
      module_tocall =
        case module do
          v when is_list(v) -> String.to_atom(Utilities.erl_list_to_iex_string(v))
          v when is_binary(v) -> String.to_atom(v)
          v when is_bitstring(v) -> String.to_atom(v)
          v when is_atom(v) -> v
        end

      # Logging.debug("called.....")

      function_tocall =
        case function_name do
          v when is_list(v) -> String.to_atom(Utilities.erl_list_to_iex_string(v))
          v when is_binary(v) -> String.to_atom(v)
          v when is_bitstring(v) -> String.to_atom(v)
          v when is_atom(v) -> v
        end

      # if Kernel.is_atom(function_name) == false do
      #   String.to_atom(function_name)
      # else
      #   function_name
      # end

      # Logging.debug("recalc arguments , appending data")
      arguments = arguments ++ [data]

      # Logging.debug("calling module: ~p function:~p args:~p", [module, function_tocall, arguments])

      Kernel.apply(module_tocall, function_tocall, arguments)
    rescue
      e ->
        Logging.error(
          "problem to call module:~p function:~p args:~p error:~p",
          [
            module,
            function_name,
            arguments,
            e
          ]
        )
    end
  end

  def for_each_non_iterable_item(iter_item, func_to_call) do
    f = fn y ->
      for_each_non_iterable_item(y, func_to_call)
    end

    case iter_item do
      ii when is_list(ii) ->
        iter_item
        |> Enum.map(fn x ->
          for_each_non_iterable_item(x, f)
        end)

      ii when is_map(ii) ->
        ii_ =
          Map.to_list(ii)
          |> Enum.map(fn x ->
            for_each_non_iterable_item(x, f)
          end)

        Map.new(ii_)

      ii when is_tuple(ii) ->
        li = Tuple.to_list(ii)
        r1 = for_each_non_iterable_item(li, f)
        List.to_tuple(r1)

      _ ->
        func_to_call.(iter_item)
    end
  end

  @spec all_user_process_nodes() :: list(Atom.t())
  def all_user_process_nodes() do
    all_active_nodes()
  end

  def is_list_of_tuples(x) do
    if is_list(x) do
      x
      |> Enum.reduce_while(true, fn x, acc ->
        case x do
          {_, _} -> {:cont, acc}
          _ -> {:halt, false}
        end
      end)
    else
      false
    end
  end
end
