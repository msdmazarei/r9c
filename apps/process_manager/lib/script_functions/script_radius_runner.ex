defmodule ProcessManager.Script.Functionalities.Radius do

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  def lua_functionalities() do
    %{
      "radius" =>
        Map.to_list(%{
          "get_attr" => &rad_get_attr/2,
          "get_str_attr" => &rad_get_str_attr/2,
          "unhide_password" => &rad_unhide_password/2,
          "response" => &rad_response/2
        })
    }
  end

  def rad_get_attr(args, state) do
    s = Process.get(:user_process_state)

    [target_attr] = args
    target_attr = Kernel.trunc(target_attr)
    search_result = :radius_attributes.find(target_attr, s[@origRadAttr])

    case search_result do
      {:error, _} ->
        {[nil], state}

      {:ok, v} ->
        Logging.debug("value:~p", [v])
        {[Script.to_lua(v)], state}
    end
  end

  def rad_get_str_attr(args, state) do
    case rad_get_attr(args, state) do
      {[nil], s} -> {[nil], s}
      {[v], s} -> {[Utilities.erl_list_to_iex_string(v)], s}
    end
  end

  def rad_response(args, state) do
    Logging.debug("called. args: ~p", [args])
    s = Process.get(:user_process_state)
    [status, attribs] = args
    attribs = Script.to_elixir(attribs)

    Logging.debug(
      "attribs:~p",
      [attribs]
    )

    rtn = %{"attribs" => attribs, "status" => status}

    {[Script.to_lua(rtn)], state}
  end

  def rad_unhide_password(args, state) do
    s = Process.get(:user_process_state)
    [password_attr_id] = args
    password_attr_id = Kernel.trunc(password_attr_id)
    secret = s[@parsedRadius].secret
    authenticator = s[@parsedRadius].authenticator
    search_result = :radius_attributes.find(password_attr_id, s[@origRadAttr])

    case search_result do
      {:error, _} ->
        {[nil], state}

      {:ok, p} ->
        passw = :radius_attributes.unhide(secret, authenticator, p)
        {[Utilities.erl_list_to_iex_string(passw)], state}
    end
  end
end
