# defprotocol Dispatcher.Protocols.DispatcherInfo do
#   def get_unit_process_module_for_message(message)
#   def get_process_name(messgae)
# end

# defimpl Dispatcher.Protocols.DispatcherInfo,
#   for: [
#     DatabaseEngine.Models.RadiusPacket
#   ] do
#   require Logger
#   require Utilities.Logging
#   alias Utilities.Logging

#   def get_unit_process_module_for_message(_message) do
#     ProcessManager.Process.RadiusProcess
#   end

#   def get_process_name(%DatabaseEngine.Models.RadiusPacket{
#         attribs: attrs
#       }) do
#     username_attr_id = 1
#     acct_session_id = 44
#     acct_multi_session_id = 50
#     u = attrs[username_attr_id]
#     sid = attrs[acct_session_id]
#     smid = attrs[acct_multi_session_id]
#     Logging.debug("attrs:~p username:~p sid: ~p smid:~p", [attrs, u, sid, smid])

#     r = u || sid || smid

#     r
#   end
# end
