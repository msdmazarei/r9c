defmodule OnlineChargingSystem.RadiusServer do
  @behaviour :radius

  alias :radius, as: Radiuslib
  alias :radius_attributes, as: RadiusAttr
  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require DatabaseEngine.Models.RadiusPacket
  require DatabaseEngine.DurableQueue
  alias DatabaseEngine.DurableQueue

  require Utilities.Conversion

  require Dispatcher.Process.VAS.UserProcess.Script

  alias Dispatcher.UserProcess.Script

  @origRadAttr "orig_rad_attrs"
  @parsedRadius "parsed_radius"
  @origRadiusRecord "orig_radius"

  @conf Application.get_env(:online_charging_system, __MODULE__)
  require Record

  Record.defrecord(:radius, code: 0, id: 0, authenticator: "", attributes: "")

  def get_radius_q() do
    case @conf[node()] do
      nil ->  nil
      v -> v[:input_Q]
    end
  end

  def get_secret_for_nas(ipaddress, port) do
    "testing123"
  end

  def hello() do
    :radius.start_link(__MODULE__, 1812)
  end

  def init(address, port) do
    Logging.debug("Called. address: ~p port: ~p", [address, port])
    {:ok, []}
  end

  def request(address, port, packet, state) do
    Logging.debug("request called params , addrss:~p port: ~p satat: ~p", [address, port, state])
    radius_input_q = get_radius_q()

    if radius_input_q == nil do
      Logging.warn(
        "No Radius input Queue defined, so we forced to drop packet from address: ~p port:~p",
        [address, port]
      )
    else
      r = :radius.codec(packet)
      binattr = radius(r, :attributes)
      requestAttributes = :radius_attributes.codec(binattr)
      secret = get_secret_for_nas(address, port)

      radpkt = %DatabaseEngine.Models.RadiusPacket{
        address: address,
        port: port,
        secret: secret,
        id: radius(r, :id),
        code: radius(r, :code),
        attribs: Map.new(requestAttributes),
        authenticator: radius(r, :authenticator)
      }

      radpkt = Utilities.Conversion.replace_all_bins_to_list(radpkt)

      Logging.debug("enqueue radius packet (address: ~p, port: ~p id: ~p, code:~p) to q:~p", [
        radpkt.address,
        radpkt.port,
        radpkt.id,
        radpkt.code,
        radius_input_q
      ])

      case DurableQueue.enqueue(radius_input_q, radpkt) do
        :nok -> Logging.warn("problem to enqueue radpkt")
        :ok -> :ok
      end
    end

    # lua_script = """

    #   function dump(o)
    #        if type(o) == 'table' then
    #           local s = '{ '
    #           for k,v in pairs(o) do
    #              if type(k) ~= 'number' then k = '"'..k..'"' end
    #              s = s .. '['..k..'] = ' .. dump(v) .. ','
    #           end
    #           return s .. '} '
    #        else
    #           return tostring(o)
    #        end
    #     end

    #   r_username = 1
    #   r_password = 2
    #   r_nas_ip = 4
    #   r_nas_port = 5
    #   r_message_authenticator = 80

    #   print("username => " , cel.radius.get_str_attr(r_username))
    #   print("password => " , cel.radius.unhide_password(r_password))
    #   print("nas_ip => " , dump(cel.radius.get_attr(r_nas_ip)))
    #   print("nas_port => " , cel.radius.get_attr(r_nas_port))
    #   print("message_authenticator=>", cel.radius.get_attr(r_message_authenticator))

    #   my_table = { [17] = "hello" , [5] = "a"}
    #   rtn = cel.radius.response(2, my_table)
    #   print(dump(rtn))
    #   return rtn

    # """

    #    simple_var="12"
    #    print(simple_var)
    #    print(type(cel.incoming_message.address))
    #    print(dump(cel.incoming_message))
    #    print(dump(cel.incoming_message.attribs[1]))
    #    print(cel.radius.get_attr())

    # script_state = %{
    #   @origRadiusRecord => r,
    #   @parsedRadius => radpkt,
    #   @origRadAttr => requestAttributes,
    #   :queues => %{}
    # }

    # r =
    #   Script.run_script(
    #     lua_script,
    #     Utilities.nested_tuple_to_list(radpkt),
    #     script_state,
    #     %{
    #       "radius" =>
    #         Map.to_list(%{
    #           "get_attr" => &rad_get_attr/2,
    #           "get_str_attr" => &rad_get_str_attr/2,
    #           "unhide_password" => &rad_unhide_password/2,
    #           "response" => &rad_response/2
    #         })
    #     }
    #   )

    # case r do
    #   {:return, rtn_value} ->
    #     [result] = rtn_value
    #     map_result = Script.to_elixir(result)
    #     radius_response(radpkt, map_result["status"], map_result["attribs"])

    #   e ->
    #     Logging.debug("error happen in script running, e: ~p", [e])
    # end
  end

  def msd_rad_attrib_to_bin(attributes) do
    attributes
    |> Enum.reduce(<<>>, fn {k, v}, acc when is_binary(v) ->
      <<acc::binary, k, byte_size(v) + 2, v::binary>>
    end)
  end

  def radius_response(radius_request_packet, resp_code, attributes) do
    resp_code = Kernel.trunc(resp_code)

    attributes =
      if is_map(attributes) do
        Map.to_list(attributes)
      else
        attributes
      end

    # [1, Utilities.to_erl_list("hello")] #:radius_attributes.codec(attributes)
    binAttrs = msd_rad_attrib_to_bin(attributes)
    l = (binAttrs |> byte_size()) + 20
    result_code = resp_code

    responseAuthenticator =
      :crypto.hash(:md5, [
        <<result_code, radius_request_packet.id, l::16>>,
        radius_request_packet.authenticator,
        binAttrs,
        radius_request_packet.secret
      ])

    response =
      radius(
        code: result_code,
        id: radius_request_packet.id,
        # radius{code = ?AccessReject, id = Id,
        authenticator: responseAuthenticator,
        attributes: binAttrs
      )

    r = {:ok, :radius.codec(response)}
    r
  end

  @spec request(
          <<_::64, _::_*8>>
          | {:radius, byte(), byte(), binary() | [byte()], binary() | [{any(), any()}]},
          binary() | char_list()
        ) :: :ok | {:error, any()}
  def request(packet, secret) do
    Logging.debug("request/2 called. ")
    r = :radius.codec(packet)

    Logging.debug("packet: ~p radcode:~p", [r, radius(r, :code)])

    binattr = radius(r, :attributes)
    requestAttributes = :radius_attributes.codec(binattr)
    Logging.debug("attrs:~p", [requestAttributes])
    userNameV = RadiusAttr.find(1, requestAttributes)
    {:ok, passwordV} = RadiusAttr.find(2, requestAttributes)
    chapPassword = RadiusAttr.find(3, requestAttributes)
    nasIp = RadiusAttr.find(4, requestAttributes)
    authenticator = radius(r, :authenticator)
    Logging.debug("secret: ~p authenticator:~p passoedv:~p", [secret, authenticator, passwordV])

    clear_password = RadiusAttr.unhide(secret, authenticator, passwordV)

    Logging.debug("reqAttr: ~p", [requestAttributes])

    Logging.debug("username: ~p nasip:~p passwod:~p chappasswodd:~p", [
      userNameV,
      nasIp,
      clear_password,
      chapPassword
    ])
  end

  def terminate(reason, state) do
    Logging.debug("terminate called. reason: ~p state: ~p", [reason, state])
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
