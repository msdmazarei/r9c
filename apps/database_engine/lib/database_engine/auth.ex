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

  @spec add_user(Map.t()) :: {:atomic, Atom.t(), Atom.t()} | {:atomic, :ok, String.t()}
  @doc """
   Adds a user by getting AuthUser struct and returens idx.
   The default permission for user will be :guest access.
    ## ERRORDESC RCE9001 -> specified key and domain is already available in database.
  """

  def add_user(user) do
    case get_user(user.domain, user.key) do
      {:atomic, {:not_found, _}} ->
        :ok

      {:atomic} ->
        :mnesia.transaction(fn ->
          idx = UUID.uuid4()
          gidx = UUID.uuid4()

          pck =
            {AuthUserTb, idx, user.key, user.domain, user |> Map.put(:idx, idx),
             %DatabaseEngine.Struct.TableInternalData{unixtime: Utilities.now()}}

          :mnesia.write({AuthGroupTb, gidx, idx})
          :mnesia.write({AuthMembershipTb, UUID.uuid4(), idx, gidx})
          :ok = :mnesia.write(pck)
          {:ok, idx}
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
          case :mnesia.index_read(AuthGroupTb, idx, :name_idx) do
            [] ->
              :pass

            [grp] ->
              :mnesia.delete({AuthGroupTb, grp |> elem(1)})
          end

          case :mnesia.index_read(AuthMembershipTb, idx, :user_idx) do
            [] ->
              :pass

            [mbsp] ->
              :mnesia.delete({AuthMembershipTb, mbsp |> elem(1)})
          end

          :mnesia.delete({AuthUserTb, idx})
      end
    end)
  end

  @spec add_permission(String.t(), String.t()) :: {:atomic, {Atom.t(), Atom.t()}} | {:atomic, :ok}
  @doc """
  adds permision to user
  """
  def add_permission(user_idx, permission) do
    :mnesia.transaction(fn ->
      case :mnesia.index_read(AuthGroupTb, user_idx, :name_idx) do
        [] ->
          {:not_found, :rce9005}

        [usergroup] ->
          case :mnesia.index_read(AuthPermissionTb, usergroup |> elem(1), :group_idx)
               |> Enum.filter(fn x ->
                 elem(x, 3) == permission
               end)
               |> length do
            0 ->
              pck = {AuthPermissionTb, UUID.uuid4(), usergroup |> elem(1), permission}

              :mnesia.write(pck)

            _ ->
              :ok
          end
      end
    end)
  end

  @spec del_permission(String.t(), String.t()) :: {:atomic, {Atom.t(), Atom.t()}} | {:atomic, :ok}
  @doc """
  removes permision from user
  """
  def del_permission(user_idx, permission) do
    :mnesia.transaction(fn ->
      case :mnesia.index_read(AuthGroupTb, user_idx, :name_idx) do
        [] ->
          {:not_found, :rce9005}

        [usergroup] ->
          targets =
            :mnesia.index_read(AuthPermissionTb, usergroup |> elem(1), :group_idx)
            |> Enum.filter(fn x ->
              elem(x, 3) == permission
            end)

          case targets |> length do
            0 ->
              :ok

            _ ->
              targets
              |> Enum.map(fn x ->
                :mnesia.delete({AuthMembershipTb, x |> elem(1)})
              end)
          end
      end
    end)
  end

  @spec has_permission(String.t(), String.t()) :: {:atomic, {Atom.t(), Atom.t()}} | {:atomic, :ok}
  @doc """
    quiries user permission
  """
  def has_permission(user_idx, permission) do
    :not_implemented
  end

  @spec get_permissions(String.t()) :: {:atomic, {Atom.t(), Atom.t()}} | {:atomic, Boolean.t()}
  @doc """
    Get list of user permissions
  """
  def get_permissions(user_idx) do
    :not_implemented
  end
end
