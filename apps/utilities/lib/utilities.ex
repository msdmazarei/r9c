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

  def randint(n) do
    :rand.uniform(n)
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

  def get_struct_type(obj) do
    if is_map(obj) do
      Map.get(obj,:__struct__,"UNKNOWN")
    else
      "UNKNOWN"
    end
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

  def is_valid_lua_table(list) when is_list(list) do
    list
    |> Enum.reduce(true, fn i, result ->
      if result == true do
        case i do
          {n, _} when is_number(n) -> result
          {s, _} when is_binary(s) -> result
          {l, _} when is_list(l) -> result
          {a, _} when is_atom(a) -> result
          {f, _} when is_function(f) -> result
          _ -> false
        end
      else
        false
      end
    end)
  end

  def is_valid_lua_table(_) do
    false
  end

  def nested_tuple_to_list(list) when is_list(list) do
    if is_valid_lua_table(list) do
      list
      |> Enum.map(fn {k, v} ->
        {nested_tuple_to_list(k), nested_tuple_to_list(v)}
      end)
    else
      list
      |> Enum.map(&nested_tuple_to_list/1)
    end
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

    r =
      Enum.reduce(
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

  def is_iterable(i) when is_map(i) or is_list(i) or is_tuple(i) do
    true
  end

  def is_iterable(i) do
    false
  end

  def iter_over_all_iterables(iterable, func, first_apply_func) when is_list(iterable) do
    iter1 =
      if first_apply_func == true do
        iterable |> Enum.map(func)
      else
        iterable
        |> Enum.map(fn i ->
          if is_iterable(i) do
            i
          else
            func.(i)
          end
        end)
      end

    iter1
    |> Enum.map(fn item ->
      if is_iterable(item) do
        i1 = iter_over_all_iterables(item, func, first_apply_func)
        func.(i1)
      else
        item
      end
    end)
  end

  def iter_over_all_iterables(iterable, func, first_apply_func)
      when is_tuple(iterable) do
    iterable
    |> Tuple.to_list()
    |> iter_over_all_iterables(func, first_apply_func)
    |> List.to_tuple()
  end

  def to_string(obj) do
    :io_lib.format("~p", [obj]) |> Utilities.erl_list_to_iex_string()
  end

  def sum_up_two_map(map, target_map) when is_map(map) and is_map(target_map) do
    map
    |> Map.to_list()
    |> Enum.reduce(
      target_map,
      fn {k, v}, acc ->
        if is_number(v) do
        end

        case v do
          _ when is_number(v) ->
            old_v = acc[k] || 0
            new_v = old_v + v
            acc |> Map.put(k, new_v)

          _ when is_map(v) ->
            acc |> Map.put(k, sum_up_two_map(v, acc[k] || %{}))

          _ ->
            acc |> Map.put(k, v)
        end
      end
    )
  end

  def iter_over_all_iterables(iterable, func, first_apply_func) when is_map(iterable) do
    iterable |> Map.to_list() |> iter_over_all_iterables(func, first_apply_func) |> Map.new()
  end

  @spec agg_binaries_till_reach_to_size([any()], any(), integer()) :: [[any()]]
  def agg_binaries_till_reach_to_size(bin_list, size_func, target_size) do
    init = [{0, []}]

    bin_list
    |> Enum.reduce(init, fn b, res_bin_list ->
      bsize = size_func.(b)

      first_proper_item_index =
        res_bin_list
        |> Enum.find_index(fn {s, _} ->
          target_size > s + bsize
        end)

      case first_proper_item_index do
        nil ->
          new_item = {size_func.(b), [b]}
          [new_item | res_bin_list]

        i ->
          {s, bl} = res_bin_list |> Enum.at(i)
          new_item = {s + bsize, [b | bl]}
          res_bin_list |> List.replace_at(i, new_item)
      end
    end)
    |> Enum.map(fn {_, l} -> l end)
  end

  def func_reomte_async_task(my_pid, my_ref, module, func, arguments) do
    init_pid = :erlang.whereis(:init)
    :erlang.process_info(init_pid, :group_leader)
    res = apply(module, func, arguments)
    send(my_pid, {my_ref, res})
  end

  def remote_async_task(node, module, func, arguments) do
    my_ref = make_ref()
    my_pid = self()

    :erlang.spawn(node, __MODULE__, :func_reomte_async_task, [
      my_pid,
      my_ref,
      module,
      func,
      arguments
    ])

    my_ref
  end

  def await_remote_task(remote_task_return, timeout, timeout_result) do
    receive do
      {^remote_task_return, res} ->
        res
    after
      timeout ->
        timeout_result
    end
  end

  def await_multi_task(
        remote_task_return_list,
        timeout,
        timeout_result,
        fin_result \\ [],
        start_time \\ Utilities.now()
      ) do
    if Utilities.now() > start_time + timeout or
         length(remote_task_return_list) == length(fin_result) do
      remote_task_return_list
      |> Enum.map(fn x ->
        found_item =
          fin_result
          |> Enum.find(fn {y, _} ->
            y == x
          end)

        found_item || timeout_result
      end)
      |> Enum.map(fn x ->
        case x do
          {_, res} -> res
          _ -> x
        end
      end)
    else
      fin_result =
        receive do
          x ->
            # check, is x something we expect that?
            is_our_result =
              remote_task_return_list
              |> Enum.find(fn item ->
                case x do
                  {^item, _} -> true
                  _ -> false
                end
              end)

            if is_our_result == nil do
              # requeue x
              send(self(), x)

              fin_result
            else
              [x | fin_result]
            end
        after
          0 ->
            fin_result
        end

      await_multi_task(
        remote_task_return_list,
        timeout,
        timeout_result,
        fin_result,
        start_time
      )
    end
  end
end
