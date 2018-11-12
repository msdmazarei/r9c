defmodule DatabaseEngine.Interface.Auth do
  @moduledoc """
    Interface for Authenticated users and permissions.
  """

  @spec get_user(String.t(), String.t(), Boolean.t()) ::
          {:atomic, Map.t()} | {:atomic, {Atom.t(), Atom.t()}}
  @doc """
    return user map based on domain and access_key

    ## ERRORDESC RCE9002 -> specified key and domain not found in database.
    ## ERRORDESC RCE9003 -> multiple records found for user and domain.
  """
  def get_user(domain, key, raw \\ false) do
    :mnesia.transaction(fn ->
      u = :mnesia.index_read(AuthUserTb, key, :key_idx)

      case u |> length do
        0 ->
          {:not_found, :rce9002}

        1 ->
          case raw do
            false ->
              u |> List.first() |> elem(4)

            true ->
              u |> List.first()
          end

        _ ->
          {:error, :rec9003}
      end
    end)
  end

  @spec add_user(Map.t()) :: String.t()
  @doc """
   Adds a user by getting AuthUser struct and returens idx.
   The default permission for user will be :guest access.
    ## ERRORDESC RCE9001 -> specified key and domain is already available in database.
  """

  def add_user(user) do
    case get_user(user.domain, user.key) do
      {:atomic, {:not_found, _}} ->
        {:atomic, :ok} =
          :mnesia.transaction(fn ->
            idx = UUID.uuid4
            pck =
              {AuthUserTb, idx, user.key, user.domain, user |> Map.put(:idx, idx),
               %DatabaseEngine.Struct.TableInternalData{unixtime: Utilities.now()}}

            :mnesia.write(pck)
          end)

      {:atomic, _} ->
        {:error, :conflict, :rce9001}
    end
  end

  @spec update_user(Map.t()) :: {:atomic, :ok}
  @doc """
  updates user basic on given struct

  """
  def update_user(user) do
    case get_user(user.domain, user.key, true) do
      {:atomic, {error, code}} ->
        {:error, error, code}

      {:atomic, old} ->
        :mnesia.transaction(fn ->
          pck =
            old
            |> put_elem(2, user.key)
            |> put_elem(3, user.domain)
            |> put_elem(4, user)
            |> put_elem(
              5,
              old |> elem(5) |> Map.put(:last_modification_unixtime, Utilities.now())
            )

          :mnesia.write(pck)
        end)
    end
  end

  @spec del_user(Map.t()) :: {:atomic, :ok}
  @doc """
  deletes user basic on user idx

  """
  def del_user(idx) do
    :mnesia.transaction(fn ->
      case :mnesia.read(AuthUserTb, idx) do
        [] ->
          {:error, :not_found, :rce9004}

         _ ->
          :mnesia.delete({AuthUserTb, idx})
      end
    end)
  end
end
