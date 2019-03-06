%% -------------------------------------------------------------------
%% This is a generated file.
%% -------------------------------------------------------------------

-module(diameter_gen_nas_application_rfc7155).

-compile({parse_transform, diameter_exprecs}).

-compile(nowarn_unused_function).

-export_records([diameter_nas_app_AAR,
		 diameter_nas_app_AAA, diameter_nas_app_RAR,
		 diameter_nas_app_RAA, diameter_nas_app_STR,
		 diameter_nas_app_STA, diameter_nas_app_ASR,
		 diameter_nas_app_ASA, diameter_nas_app_ACR,
		 diameter_nas_app_ACA, 'diameter_nas_app_CHAP-Auth',
		 diameter_nas_app_Tunneling,
		 'diameter_nas_app_Proxy-Info',
		 'diameter_nas_app_Failed-AVP',
		 'diameter_nas_app_Experimental-Result',
		 'diameter_nas_app_Vendor-Specific-Application-Id']).

-record(diameter_nas_app_AAR,
	{'Session-Id', 'Auth-Application-Id', 'Origin-Host',
	 'Origin-Realm', 'Destination-Realm',
	 'Auth-Request-Type', 'Destination-Host' = [],
	 'NAS-Identifier' = [], 'NAS-IP-Address' = [],
	 'NAS-IPv6-Address' = [], 'NAS-Port' = [],
	 'NAS-Port-Id' = [], 'NAS-Port-Type' = [],
	 'Origin-AAA-Protocol' = [], 'Origin-State-Id' = [],
	 'Port-Limit' = [], 'User-Name' = [],
	 'User-Password' = [], 'Service-Type' = [], 'State' = [],
	 'Authorization-Lifetime' = [], 'Auth-Grace-Period' = [],
	 'Auth-Session-State' = [], 'Callback-Number' = [],
	 'Called-Station-Id' = [], 'Calling-Station-Id' = [],
	 'Originating-Line-Info' = [], 'Connect-Info' = [],
	 'CHAP-Auth' = [], 'CHAP-Challenge' = [],
	 'Framed-Compression' = [], 'Framed-Interface-Id' = [],
	 'Framed-IP-Address' = [], 'Framed-IPv6-Prefix' = [],
	 'Framed-IP-Netmask' = [], 'Framed-MTU' = [],
	 'Framed-Protocol' = [], 'ARAP-Password' = [],
	 'ARAP-Security' = [], 'ARAP-Security-Data' = [],
	 'Login-IP-Host' = [], 'Login-IPv6-Host' = [],
	 'Login-LAT-Group' = [], 'Login-LAT-Node' = [],
	 'Login-LAT-Port' = [], 'Login-LAT-Service' = [],
	 'Tunneling' = [], 'Proxy-Info' = [],
	 'Route-Record' = [], 'AVP' = []}).

-record(diameter_nas_app_AAA,
	{'Session-Id', 'Auth-Application-Id',
	 'Auth-Request-Type', 'Result-Code', 'Origin-Host',
	 'Origin-Realm', 'User-Name' = [], 'Service-Type' = [],
	 'Class' = [], 'Configuration-Token' = [],
	 'Acct-Interim-Interval' = [], 'Error-Message' = [],
	 'Error-Reporting-Host' = [], 'Failed-AVP' = [],
	 'Idle-Timeout' = [], 'Authorization-Lifetime' = [],
	 'Auth-Grace-Period' = [], 'Auth-Session-State' = [],
	 'Re-Auth-Request-Type' = [],
	 'Multi-Round-Time-Out' = [], 'Session-Timeout' = [],
	 'State' = [], 'Reply-Message' = [],
	 'Origin-AAA-Protocol' = [], 'Origin-State-Id' = [],
	 'Filter-Id' = [], 'Password-Retry' = [],
	 'Port-Limit' = [], 'Prompt' = [],
	 'ARAP-Challenge-Response' = [], 'ARAP-Features' = [],
	 'ARAP-Security' = [], 'ARAP-Security-Data' = [],
	 'ARAP-Zone-Access' = [], 'Callback-Id' = [],
	 'Callback-Number' = [], 'Framed-Appletalk-Link' = [],
	 'Framed-Appletalk-Network' = [],
	 'Framed-Appletalk-Zone' = [], 'Framed-Compression' = [],
	 'Framed-Interface-Id' = [], 'Framed-IP-Address' = [],
	 'Framed-IPv6-Prefix' = [], 'Framed-IPv6-Pool' = [],
	 'Framed-IPv6-Route' = [], 'Framed-IP-Netmask' = [],
	 'Framed-Route' = [], 'Framed-Pool' = [],
	 'Framed-IPX-Network' = [], 'Framed-MTU' = [],
	 'Framed-Protocol' = [], 'Framed-Routing' = [],
	 'Login-IP-Host' = [], 'Login-IPv6-Host' = [],
	 'Login-LAT-Group' = [], 'Login-LAT-Node' = [],
	 'Login-LAT-Port' = [], 'Login-LAT-Service' = [],
	 'Login-Service' = [], 'Login-TCP-Port' = [],
	 'NAS-Filter-Rule' = [], 'QoS-Filter-Rule' = [],
	 'Tunneling' = [], 'Redirect-Host' = [],
	 'Redirect-Host-Usage' = [],
	 'Redirect-Max-Cache-Time' = [], 'Proxy-Info' = [],
	 'AVP' = []}).

-record(diameter_nas_app_RAR,
	{'Session-Id', 'Origin-Host', 'Origin-Realm',
	 'Destination-Realm', 'Destination-Host',
	 'Auth-Application-Id', 'Re-Auth-Request-Type',
	 'User-Name' = [], 'Origin-AAA-Protocol' = [],
	 'Origin-State-Id' = [], 'NAS-Identifier' = [],
	 'NAS-IP-Address' = [], 'NAS-IPv6-Address' = [],
	 'NAS-Port' = [], 'NAS-Port-Id' = [],
	 'NAS-Port-Type' = [], 'Service-Type' = [],
	 'Framed-IP-Address' = [], 'Framed-IPv6-Prefix' = [],
	 'Framed-Interface-Id' = [], 'Called-Station-Id' = [],
	 'Calling-Station-Id' = [], 'Originating-Line-Info' = [],
	 'Acct-Session-Id' = [], 'Acct-Multi-Session-Id' = [],
	 'State' = [], 'Class' = [], 'Reply-Message' = [],
	 'Proxy-Info' = [], 'Route-Record' = [], 'AVP' = []}).

-record(diameter_nas_app_RAA,
	{'Session-Id', 'Result-Code', 'Origin-Host',
	 'Origin-Realm', 'User-Name' = [],
	 'Origin-AAA-Protocol' = [], 'Origin-State-Id' = [],
	 'Error-Message' = [], 'Error-Reporting-Host' = [],
	 'Failed-AVP' = [], 'Redirect-Host' = [],
	 'Redirect-Host-Usage' = [],
	 'Redirect-Max-Cache-Time' = [], 'Service-Type' = [],
	 'Configuration-Token' = [], 'Idle-Timeout' = [],
	 'Authorization-Lifetime' = [], 'Auth-Grace-Period' = [],
	 'Re-Auth-Request-Type' = [], 'State' = [], 'Class' = [],
	 'Reply-Message' = [], 'Prompt' = [], 'Proxy-Info' = [],
	 'AVP' = []}).

-record(diameter_nas_app_STR,
	{'Session-Id', 'Origin-Host', 'Origin-Realm',
	 'Destination-Realm', 'Auth-Application-Id',
	 'Termination-Cause', 'User-Name' = [],
	 'Destination-Host' = [], 'Class' = [],
	 'Origin-AAA-Protocol' = [], 'Origin-State-Id' = [],
	 'Proxy-Info' = [], 'Route-Record' = [], 'AVP' = []}).

-record(diameter_nas_app_STA,
	{'Session-Id', 'Result-Code', 'Origin-Host',
	 'Origin-Realm', 'User-Name' = [], 'Class' = [],
	 'Error-Message' = [], 'Error-Reporting-Host' = [],
	 'Failed-AVP' = [], 'Origin-AAA-Protocol' = [],
	 'Origin-State-Id' = [], 'Redirect-Host' = [],
	 'Redirect-Host-Usage' = [],
	 'Redirect-Max-Cache-Time' = [], 'Proxy-Info' = [],
	 'AVP' = []}).

-record(diameter_nas_app_ASR,
	{'Session-Id', 'Origin-Host', 'Origin-Realm',
	 'Destination-Realm', 'Destination-Host',
	 'Auth-Application-Id', 'User-Name' = [],
	 'Origin-AAA-Protocol' = [], 'Origin-State-Id' = [],
	 'NAS-Identifier' = [], 'NAS-IP-Address' = [],
	 'NAS-IPv6-Address' = [], 'NAS-Port' = [],
	 'NAS-Port-Id' = [], 'NAS-Port-Type' = [],
	 'Service-Type' = [], 'Framed-IP-Address' = [],
	 'Framed-IPv6-Prefix' = [], 'Framed-Interface-Id' = [],
	 'Called-Station-Id' = [], 'Calling-Station-Id' = [],
	 'Originating-Line-Info' = [], 'Acct-Session-Id' = [],
	 'Acct-Multi-Session-Id' = [], 'State' = [],
	 'Class' = [], 'Reply-Message' = [], 'Proxy-Info' = [],
	 'Route-Record' = [], 'AVP' = []}).

-record(diameter_nas_app_ASA,
	{'Session-Id', 'Result-Code', 'Origin-Host',
	 'Origin-Realm', 'User-Name' = [],
	 'Origin-AAA-Protocol' = [], 'Origin-State-Id' = [],
	 'State' = [], 'Error-Message' = [],
	 'Error-Reporting-Host' = [], 'Failed-AVP' = [],
	 'Redirect-Host' = [], 'Redirect-Host-Usage' = [],
	 'Redirect-Max-Cache-Time' = [], 'Proxy-Info' = [],
	 'AVP' = []}).

-record(diameter_nas_app_ACR,
	{'Session-Id', 'Origin-Host', 'Origin-Realm',
	 'Destination-Realm', 'Accounting-Record-Type',
	 'Accounting-Record-Number', 'Acct-Application-Id',
	 'User-Name' = [], 'Accounting-Sub-Session-Id' = [],
	 'Acct-Session-Id' = [], 'Acct-Multi-Session-Id' = [],
	 'Origin-AAA-Protocol' = [], 'Origin-State-Id' = [],
	 'Destination-Host' = [], 'Event-Timestamp' = [],
	 'Acct-Delay-Time' = [], 'NAS-Identifier' = [],
	 'NAS-IP-Address' = [], 'NAS-IPv6-Address' = [],
	 'NAS-Port' = [], 'NAS-Port-Id' = [],
	 'NAS-Port-Type' = [], 'Class' = [], 'Service-Type' = [],
	 'Termination-Cause' = [],
	 'Accounting-Input-Octets' = [],
	 'Accounting-Input-Packets' = [],
	 'Accounting-Output-Octets' = [],
	 'Accounting-Output-Packets' = [], 'Acct-Authentic' = [],
	 'Accounting-Auth-Method' = [], 'Acct-Link-Count' = [],
	 'Acct-Session-Time' = [], 'Acct-Tunnel-Connection' = [],
	 'Acct-Tunnel-Packets-Lost' = [], 'Callback-Id' = [],
	 'Callback-Number' = [], 'Called-Station-Id' = [],
	 'Calling-Station-Id' = [], 'Connect-Info' = [],
	 'Originating-Line-Info' = [],
	 'Authorization-Lifetime' = [], 'Session-Timeout' = [],
	 'Idle-Timeout' = [], 'Port-Limit' = [],
	 'Accounting-Realtime-Required' = [],
	 'Acct-Interim-Interval' = [], 'Filter-Id' = [],
	 'NAS-Filter-Rule' = [], 'QoS-Filter-Rule' = [],
	 'Framed-Appletalk-Link' = [],
	 'Framed-Appletalk-Network' = [],
	 'Framed-Appletalk-Zone' = [], 'Framed-Compression' = [],
	 'Framed-Interface-Id' = [], 'Framed-IP-Address' = [],
	 'Framed-IP-Netmask' = [], 'Framed-IPv6-Prefix' = [],
	 'Framed-IPv6-Pool' = [], 'Framed-IPv6-Route' = [],
	 'Framed-IPX-Network' = [], 'Framed-MTU' = [],
	 'Framed-Pool' = [], 'Framed-Protocol' = [],
	 'Framed-Route' = [], 'Framed-Routing' = [],
	 'Login-IP-Host' = [], 'Login-IPv6-Host' = [],
	 'Login-LAT-Group' = [], 'Login-LAT-Node' = [],
	 'Login-LAT-Port' = [], 'Login-LAT-Service' = [],
	 'Login-Service' = [], 'Login-TCP-Port' = [],
	 'Tunneling' = [], 'Proxy-Info' = [],
	 'Route-Record' = [], 'AVP' = []}).

-record(diameter_nas_app_ACA,
	{'Session-Id', 'Result-Code', 'Origin-Host',
	 'Origin-Realm', 'Accounting-Record-Type',
	 'Accounting-Record-Number', 'Acct-Application-Id',
	 'User-Name' = [], 'Accounting-Sub-Session-Id' = [],
	 'Acct-Session-Id' = [], 'Acct-Multi-Session-Id' = [],
	 'Event-Timestamp' = [], 'Error-Message' = [],
	 'Error-Reporting-Host' = [], 'Failed-AVP' = [],
	 'Origin-AAA-Protocol' = [], 'Origin-State-Id' = [],
	 'NAS-Identifier' = [], 'NAS-IP-Address' = [],
	 'NAS-IPv6-Address' = [], 'NAS-Port' = [],
	 'NAS-Port-Id' = [], 'NAS-Port-Type' = [],
	 'Service-Type' = [], 'Termination-Cause' = [],
	 'Accounting-Realtime-Required' = [],
	 'Acct-Interim-Interval' = [], 'Class' = [],
	 'Proxy-Info' = [], 'AVP' = []}).

-record('diameter_nas_app_CHAP-Auth',
	{'CHAP-Algorithm', 'CHAP-Ident', 'CHAP-Response' = [],
	 'AVP' = []}).

-record(diameter_nas_app_Tunneling,
	{'Tunnel-Type', 'Tunnel-Medium-Type',
	 'Tunnel-Client-Endpoint', 'Tunnel-Server-Endpoint',
	 'Tunnel-Preference' = [], 'Tunnel-Client-Auth-Id' = [],
	 'Tunnel-Server-Auth-Id' = [],
	 'Tunnel-Assignment-Id' = [], 'Tunnel-Password' = [],
	 'Tunnel-Private-Group-Id' = []}).

-record('diameter_nas_app_Proxy-Info',
	{'Proxy-Host', 'Proxy-State', 'AVP' = []}).

-record('diameter_nas_app_Failed-AVP', {'AVP' = []}).

-record('diameter_nas_app_Experimental-Result',
	{'Vendor-Id', 'Experimental-Result-Code'}).

-record('diameter_nas_app_Vendor-Specific-Application-Id',
	{'Vendor-Id', 'Auth-Application-Id' = [],
	 'Acct-Application-Id' = []}).

-export([name/0, id/0, vendor_id/0, vendor_name/0,
	 decode_avps/3, encode_avps/3, grouped_avp/4, msg_name/2,
	 msg_header/1, rec2msg/1, msg2rec/1, name2rec/1,
	 avp_name/2, avp_arity/1, avp_arity/2, avp_header/1,
	 avp/4, enumerated_avp/3, empty_value/2, dict/0]).

-include_lib("diameter/include/diameter.hrl").

-include_lib("diameter/include/diameter_gen.hrl").

name() -> diameter_gen_nas_application_rfc7155.

id() -> 1.

vendor_id() -> 0.

vendor_name() -> 'IETF'.

msg_name(265, true) -> 'AAR';
msg_name(265, false) -> 'AAA';
msg_name(271, true) -> 'ACR';
msg_name(271, false) -> 'ACA';
msg_name(258, true) -> 'RAR';
msg_name(258, false) -> 'RAA';
msg_name(274, true) -> 'ASR';
msg_name(274, false) -> 'ASA';
msg_name(275, true) -> 'STR';
msg_name(275, false) -> 'STA';
msg_name(_, _) -> ''.

msg_header('AAR') -> {265, 192, 1};
msg_header('AAA') -> {265, 64, 1};
msg_header('RAR') -> {258, 192, 1};
msg_header('RAA') -> {258, 64, 1};
msg_header('STR') -> {275, 192, 1};
msg_header('STA') -> {275, 64, 1};
msg_header('ASR') -> {274, 192, 1};
msg_header('ASA') -> {274, 64, 1};
msg_header('ACR') -> {271, 192, 1};
msg_header('ACA') -> {271, 64, 1};
msg_header(_) -> erlang:error(badarg).

rec2msg(diameter_nas_app_AAR) -> 'AAR';
rec2msg(diameter_nas_app_AAA) -> 'AAA';
rec2msg(diameter_nas_app_RAR) -> 'RAR';
rec2msg(diameter_nas_app_RAA) -> 'RAA';
rec2msg(diameter_nas_app_STR) -> 'STR';
rec2msg(diameter_nas_app_STA) -> 'STA';
rec2msg(diameter_nas_app_ASR) -> 'ASR';
rec2msg(diameter_nas_app_ASA) -> 'ASA';
rec2msg(diameter_nas_app_ACR) -> 'ACR';
rec2msg(diameter_nas_app_ACA) -> 'ACA';
rec2msg(_) -> erlang:error(badarg).

msg2rec('AAR') -> diameter_nas_app_AAR;
msg2rec('AAA') -> diameter_nas_app_AAA;
msg2rec('RAR') -> diameter_nas_app_RAR;
msg2rec('RAA') -> diameter_nas_app_RAA;
msg2rec('STR') -> diameter_nas_app_STR;
msg2rec('STA') -> diameter_nas_app_STA;
msg2rec('ASR') -> diameter_nas_app_ASR;
msg2rec('ASA') -> diameter_nas_app_ASA;
msg2rec('ACR') -> diameter_nas_app_ACR;
msg2rec('ACA') -> diameter_nas_app_ACA;
msg2rec(_) -> erlang:error(badarg).

name2rec('CHAP-Auth') -> 'diameter_nas_app_CHAP-Auth';
name2rec('Tunneling') -> diameter_nas_app_Tunneling;
name2rec('Proxy-Info') -> 'diameter_nas_app_Proxy-Info';
name2rec('Failed-AVP') -> 'diameter_nas_app_Failed-AVP';
name2rec('Experimental-Result') ->
    'diameter_nas_app_Experimental-Result';
name2rec('Vendor-Specific-Application-Id') ->
    'diameter_nas_app_Vendor-Specific-Application-Id';
name2rec(T) -> msg2rec(T).

avp_name(84, 0) ->
    {'ARAP-Challenge-Response', 'OctetString'};
avp_name(71, 0) -> {'ARAP-Features', 'OctetString'};
avp_name(70, 0) -> {'ARAP-Password', 'OctetString'};
avp_name(73, 0) -> {'ARAP-Security', 'Unsigned32'};
avp_name(74, 0) ->
    {'ARAP-Security-Data', 'OctetString'};
avp_name(72, 0) -> {'ARAP-Zone-Access', 'Enumerated'};
avp_name(406, 0) ->
    {'Accounting-Auth-Method', 'Enumerated'};
avp_name(363, 0) ->
    {'Accounting-Input-Octets', 'Unsigned64'};
avp_name(365, 0) ->
    {'Accounting-Input-Packets', 'Unsigned64'};
avp_name(364, 0) ->
    {'Accounting-Output-Octets', 'Unsigned64'};
avp_name(366, 0) ->
    {'Accounting-Output-Packets', 'Unsigned64'};
avp_name(45, 0) -> {'Acct-Authentic', 'Unsigned32'};
avp_name(41, 0) -> {'Acct-Delay-Time', 'Unsigned32'};
avp_name(51, 0) -> {'Acct-Link-Count', 'Unsigned32'};
avp_name(46, 0) -> {'Acct-Session-Time', 'Unsigned32'};
avp_name(68, 0) ->
    {'Acct-Tunnel-Connection', 'OctetString'};
avp_name(86, 0) ->
    {'Acct-Tunnel-Packets-Lost', 'Unsigned32'};
avp_name(403, 0) -> {'CHAP-Algorithm', 'Enumerated'};
avp_name(402, 0) -> {'CHAP-Auth', 'Grouped'};
avp_name(60, 0) -> {'CHAP-Challenge', 'OctetString'};
avp_name(404, 0) -> {'CHAP-Ident', 'OctetString'};
avp_name(405, 0) -> {'CHAP-Response', 'OctetString'};
avp_name(20, 0) -> {'Callback-Id', 'UTF8String'};
avp_name(19, 0) -> {'Callback-Number', 'UTF8String'};
avp_name(30, 0) -> {'Called-Station-Id', 'UTF8String'};
avp_name(31, 0) -> {'Calling-Station-Id', 'UTF8String'};
avp_name(78, 0) ->
    {'Configuration-Token', 'OctetString'};
avp_name(77, 0) -> {'Connect-Info', 'UTF8String'};
avp_name(11, 0) -> {'Filter-Id', 'UTF8String'};
avp_name(37, 0) ->
    {'Framed-Appletalk-Link', 'Unsigned32'};
avp_name(38, 0) ->
    {'Framed-Appletalk-Network', 'Unsigned32'};
avp_name(39, undefined) ->
    {'Framed-Appletalk-Zone', 'OctetString'};
avp_name(13, 0) -> {'Framed-Compression', 'Enumerated'};
avp_name(8, 0) -> {'Framed-IP-Address', 'OctetString'};
avp_name(9, 0) -> {'Framed-IP-Netmask', 'OctetString'};
avp_name(23, 0) -> {'Framed-IPX-Network', 'Unsigned32'};
avp_name(100, 0) -> {'Framed-IPv6-Pool', 'OctetString'};
avp_name(97, 0) ->
    {'Framed-IPv6-Prefix', 'OctetString'};
avp_name(99, 0) -> {'Framed-IPv6-Route', 'UTF8String'};
avp_name(96, 0) ->
    {'Framed-Interface-Id', 'Unsigned64'};
avp_name(12, 0) -> {'Framed-MTU', 'Unsigned32'};
avp_name(88, 0) -> {'Framed-Pool', 'OctetString'};
avp_name(7, 0) -> {'Framed-Protocol', 'Enumerated'};
avp_name(22, 0) -> {'Framed-Route', 'UTF8String'};
avp_name(10, 0) -> {'Framed-Routing', 'Enumerated'};
avp_name(28, 0) -> {'Idle-Timeout', 'Unsigned32'};
avp_name(14, 0) -> {'Login-IP-Host', 'OctetString'};
avp_name(98, 0) -> {'Login-IPv6-Host', 'OctetString'};
avp_name(36, 0) -> {'Login-LAT-Group', 'OctetString'};
avp_name(35, 0) -> {'Login-LAT-Node', 'OctetString'};
avp_name(63, 0) -> {'Login-LAT-Port', 'OctetString'};
avp_name(34, 0) -> {'Login-LAT-Service', 'OctetString'};
avp_name(15, 0) -> {'Login-Service', 'Enumerated'};
avp_name(16, 0) -> {'Login-TCP-Port', 'Unsigned32'};
avp_name(400, 0) -> {'NAS-Filter-Rule', 'IPFilterRule'};
avp_name(4, undefined) ->
    {'NAS-IP-Address', 'OctetString'};
avp_name(95, undefined) ->
    {'NAS-IPv6-Address', 'OctetString'};
avp_name(32, undefined) ->
    {'NAS-Identifier', 'UTF8String'};
avp_name(5, 0) -> {'NAS-Port', 'Unsigned32'};
avp_name(87, 0) -> {'NAS-Port-Id', 'UTF8String'};
avp_name(61, 0) -> {'NAS-Port-Type', 'Enumerated'};
avp_name(408, undefined) ->
    {'Origin-AAA-Protocol', 'Enumerated'};
avp_name(94, 0) ->
    {'Originating-Line-Info', 'OctetString'};
avp_name(75, 0) -> {'Password-Retry', 'Unsigned32'};
avp_name(62, 0) -> {'Port-Limit', 'Unsigned32'};
avp_name(76, 0) -> {'Prompt', 'Enumerated'};
avp_name(407, undefined) ->
    {'QoS-Filter-Rule', 'QoSFilterRule'};
avp_name(18, 0) -> {'Reply-Message', 'UTF8String'};
avp_name(6, 0) -> {'Service-Type', 'Enumerated'};
avp_name(24, undefined) -> {'State', 'OctetString'};
avp_name(82, 0) ->
    {'Tunnel-Assignment-Id', 'OctetString'};
avp_name(90, 0) ->
    {'Tunnel-Client-Auth-Id', 'UTF8String'};
avp_name(66, 0) ->
    {'Tunnel-Client-Endpoint', 'UTF8String'};
avp_name(65, 0) -> {'Tunnel-Medium-Type', 'Enumerated'};
avp_name(69, 0) -> {'Tunnel-Password', 'OctetString'};
avp_name(83, 0) -> {'Tunnel-Preference', 'Unsigned32'};
avp_name(81, 0) ->
    {'Tunnel-Private-Group-Id', 'OctetString'};
avp_name(91, 0) ->
    {'Tunnel-Server-Auth-Id', 'UTF8String'};
avp_name(67, 0) ->
    {'Tunnel-Server-Endpoint', 'UTF8String'};
avp_name(64, 0) -> {'Tunnel-Type', 'Enumerated'};
avp_name(401, 0) -> {'Tunneling', 'Grouped'};
avp_name(2, 0) -> {'User-Password', 'OctetString'};
avp_name(483, undefined) ->
    {'Accounting-Realtime-Required', 'Enumerated'};
avp_name(485, undefined) ->
    {'Accounting-Record-Number', 'Unsigned32'};
avp_name(480, undefined) ->
    {'Accounting-Record-Type', 'Enumerated'};
avp_name(287, undefined) ->
    {'Accounting-Sub-Session-Id', 'Unsigned64'};
avp_name(259, undefined) ->
    {'Acct-Application-Id', 'Unsigned32'};
avp_name(85, undefined) ->
    {'Acct-Interim-Interval', 'Unsigned32'};
avp_name(50, undefined) ->
    {'Acct-Multi-Session-Id', 'UTF8String'};
avp_name(44, undefined) ->
    {'Acct-Session-Id', 'OctetString'};
avp_name(258, undefined) ->
    {'Auth-Application-Id', 'Unsigned32'};
avp_name(276, undefined) ->
    {'Auth-Grace-Period', 'Unsigned32'};
avp_name(274, undefined) ->
    {'Auth-Request-Type', 'Enumerated'};
avp_name(277, undefined) ->
    {'Auth-Session-State', 'Enumerated'};
avp_name(291, undefined) ->
    {'Authorization-Lifetime', 'Unsigned32'};
avp_name(25, undefined) -> {'Class', 'OctetString'};
avp_name(293, undefined) ->
    {'Destination-Host', 'DiameterIdentity'};
avp_name(283, undefined) ->
    {'Destination-Realm', 'DiameterIdentity'};
avp_name(273, undefined) ->
    {'Disconnect-Cause', 'Enumerated'};
avp_name(281, undefined) ->
    {'Error-Message', 'UTF8String'};
avp_name(294, undefined) ->
    {'Error-Reporting-Host', 'DiameterIdentity'};
avp_name(55, undefined) -> {'Event-Timestamp', 'Time'};
avp_name(297, undefined) ->
    {'Experimental-Result', 'Grouped'};
avp_name(298, undefined) ->
    {'Experimental-Result-Code', 'Unsigned32'};
avp_name(279, undefined) -> {'Failed-AVP', 'Grouped'};
avp_name(267, undefined) ->
    {'Firmware-Revision', 'Unsigned32'};
avp_name(257, undefined) ->
    {'Host-IP-Address', 'Address'};
avp_name(299, undefined) ->
    {'Inband-Security-Id', 'Unsigned32'};
avp_name(272, undefined) ->
    {'Multi-Round-Time-Out', 'Unsigned32'};
avp_name(264, undefined) ->
    {'Origin-Host', 'DiameterIdentity'};
avp_name(296, undefined) ->
    {'Origin-Realm', 'DiameterIdentity'};
avp_name(278, undefined) ->
    {'Origin-State-Id', 'Unsigned32'};
avp_name(269, undefined) ->
    {'Product-Name', 'UTF8String'};
avp_name(280, undefined) ->
    {'Proxy-Host', 'DiameterIdentity'};
avp_name(284, undefined) -> {'Proxy-Info', 'Grouped'};
avp_name(33, undefined) ->
    {'Proxy-State', 'OctetString'};
avp_name(285, undefined) ->
    {'Re-Auth-Request-Type', 'Enumerated'};
avp_name(292, undefined) ->
    {'Redirect-Host', 'DiameterURI'};
avp_name(261, undefined) ->
    {'Redirect-Host-Usage', 'Enumerated'};
avp_name(262, undefined) ->
    {'Redirect-Max-Cache-Time', 'Unsigned32'};
avp_name(268, undefined) ->
    {'Result-Code', 'Unsigned32'};
avp_name(282, undefined) ->
    {'Route-Record', 'DiameterIdentity'};
avp_name(270, undefined) ->
    {'Session-Binding', 'Unsigned32'};
avp_name(263, undefined) ->
    {'Session-Id', 'UTF8String'};
avp_name(271, undefined) ->
    {'Session-Server-Failover', 'Enumerated'};
avp_name(27, undefined) ->
    {'Session-Timeout', 'Unsigned32'};
avp_name(265, undefined) ->
    {'Supported-Vendor-Id', 'Unsigned32'};
avp_name(295, undefined) ->
    {'Termination-Cause', 'Enumerated'};
avp_name(1, undefined) -> {'User-Name', 'UTF8String'};
avp_name(266, undefined) -> {'Vendor-Id', 'Unsigned32'};
avp_name(260, undefined) ->
    {'Vendor-Specific-Application-Id', 'Grouped'};
avp_name(_, _) -> 'AVP'.

avp_arity('AAR') ->
    [{'Session-Id', 1}, {'Auth-Application-Id', 1},
     {'Origin-Host', 1}, {'Origin-Realm', 1},
     {'Destination-Realm', 1}, {'Auth-Request-Type', 1},
     {'Destination-Host', {0, 1}},
     {'NAS-Identifier', {0, 1}}, {'NAS-IP-Address', {0, 1}},
     {'NAS-IPv6-Address', {0, 1}}, {'NAS-Port', {0, 1}},
     {'NAS-Port-Id', {0, 1}}, {'NAS-Port-Type', {0, 1}},
     {'Origin-AAA-Protocol', {0, 1}},
     {'Origin-State-Id', {0, 1}}, {'Port-Limit', {0, 1}},
     {'User-Name', {0, 1}}, {'User-Password', {0, 1}},
     {'Service-Type', {0, 1}}, {'State', {0, 1}},
     {'Authorization-Lifetime', {0, 1}},
     {'Auth-Grace-Period', {0, 1}},
     {'Auth-Session-State', {0, 1}},
     {'Callback-Number', {0, 1}},
     {'Called-Station-Id', {0, 1}},
     {'Calling-Station-Id', {0, 1}},
     {'Originating-Line-Info', {0, 1}},
     {'Connect-Info', {0, 1}}, {'CHAP-Auth', {0, 1}},
     {'CHAP-Challenge', {0, 1}},
     {'Framed-Compression', {0, '*'}},
     {'Framed-Interface-Id', {0, 1}},
     {'Framed-IP-Address', {0, 1}},
     {'Framed-IPv6-Prefix', {0, '*'}},
     {'Framed-IP-Netmask', {0, 1}}, {'Framed-MTU', {0, 1}},
     {'Framed-Protocol', {0, 1}}, {'ARAP-Password', {0, 1}},
     {'ARAP-Security', {0, 1}},
     {'ARAP-Security-Data', {0, '*'}},
     {'Login-IP-Host', {0, '*'}},
     {'Login-IPv6-Host', {0, '*'}},
     {'Login-LAT-Group', {0, 1}}, {'Login-LAT-Node', {0, 1}},
     {'Login-LAT-Port', {0, 1}},
     {'Login-LAT-Service', {0, 1}}, {'Tunneling', {0, '*'}},
     {'Proxy-Info', {0, '*'}}, {'Route-Record', {0, '*'}},
     {'AVP', {0, '*'}}];
avp_arity('AAA') ->
    [{'Session-Id', 1}, {'Auth-Application-Id', 1},
     {'Auth-Request-Type', 1}, {'Result-Code', 1},
     {'Origin-Host', 1}, {'Origin-Realm', 1},
     {'User-Name', {0, 1}}, {'Service-Type', {0, 1}},
     {'Class', {0, '*'}}, {'Configuration-Token', {0, '*'}},
     {'Acct-Interim-Interval', {0, 1}},
     {'Error-Message', {0, 1}},
     {'Error-Reporting-Host', {0, 1}},
     {'Failed-AVP', {0, '*'}}, {'Idle-Timeout', {0, 1}},
     {'Authorization-Lifetime', {0, 1}},
     {'Auth-Grace-Period', {0, 1}},
     {'Auth-Session-State', {0, 1}},
     {'Re-Auth-Request-Type', {0, 1}},
     {'Multi-Round-Time-Out', {0, 1}},
     {'Session-Timeout', {0, 1}}, {'State', {0, 1}},
     {'Reply-Message', {0, '*'}},
     {'Origin-AAA-Protocol', {0, 1}},
     {'Origin-State-Id', {0, 1}}, {'Filter-Id', {0, '*'}},
     {'Password-Retry', {0, 1}}, {'Port-Limit', {0, 1}},
     {'Prompt', {0, 1}}, {'ARAP-Challenge-Response', {0, 1}},
     {'ARAP-Features', {0, 1}}, {'ARAP-Security', {0, 1}},
     {'ARAP-Security-Data', {0, '*'}},
     {'ARAP-Zone-Access', {0, 1}}, {'Callback-Id', {0, 1}},
     {'Callback-Number', {0, 1}},
     {'Framed-Appletalk-Link', {0, 1}},
     {'Framed-Appletalk-Network', {0, '*'}},
     {'Framed-Appletalk-Zone', {0, 1}},
     {'Framed-Compression', {0, '*'}},
     {'Framed-Interface-Id', {0, 1}},
     {'Framed-IP-Address', {0, 1}},
     {'Framed-IPv6-Prefix', {0, '*'}},
     {'Framed-IPv6-Pool', {0, 1}},
     {'Framed-IPv6-Route', {0, '*'}},
     {'Framed-IP-Netmask', {0, 1}},
     {'Framed-Route', {0, '*'}}, {'Framed-Pool', {0, 1}},
     {'Framed-IPX-Network', {0, 1}}, {'Framed-MTU', {0, 1}},
     {'Framed-Protocol', {0, 1}}, {'Framed-Routing', {0, 1}},
     {'Login-IP-Host', {0, '*'}},
     {'Login-IPv6-Host', {0, '*'}},
     {'Login-LAT-Group', {0, 1}}, {'Login-LAT-Node', {0, 1}},
     {'Login-LAT-Port', {0, 1}},
     {'Login-LAT-Service', {0, 1}},
     {'Login-Service', {0, 1}}, {'Login-TCP-Port', {0, 1}},
     {'NAS-Filter-Rule', {0, '*'}},
     {'QoS-Filter-Rule', {0, '*'}}, {'Tunneling', {0, '*'}},
     {'Redirect-Host', {0, '*'}},
     {'Redirect-Host-Usage', {0, 1}},
     {'Redirect-Max-Cache-Time', {0, 1}},
     {'Proxy-Info', {0, '*'}}, {'AVP', {0, '*'}}];
avp_arity('RAR') ->
    [{'Session-Id', 1}, {'Origin-Host', 1},
     {'Origin-Realm', 1}, {'Destination-Realm', 1},
     {'Destination-Host', 1}, {'Auth-Application-Id', 1},
     {'Re-Auth-Request-Type', 1}, {'User-Name', {0, 1}},
     {'Origin-AAA-Protocol', {0, 1}},
     {'Origin-State-Id', {0, 1}}, {'NAS-Identifier', {0, 1}},
     {'NAS-IP-Address', {0, 1}},
     {'NAS-IPv6-Address', {0, 1}}, {'NAS-Port', {0, 1}},
     {'NAS-Port-Id', {0, 1}}, {'NAS-Port-Type', {0, 1}},
     {'Service-Type', {0, 1}}, {'Framed-IP-Address', {0, 1}},
     {'Framed-IPv6-Prefix', {0, 1}},
     {'Framed-Interface-Id', {0, 1}},
     {'Called-Station-Id', {0, 1}},
     {'Calling-Station-Id', {0, 1}},
     {'Originating-Line-Info', {0, 1}},
     {'Acct-Session-Id', {0, 1}},
     {'Acct-Multi-Session-Id', {0, 1}}, {'State', {0, 1}},
     {'Class', {0, '*'}}, {'Reply-Message', {0, 1}},
     {'Proxy-Info', {0, '*'}}, {'Route-Record', {0, '*'}},
     {'AVP', {0, '*'}}];
avp_arity('RAA') ->
    [{'Session-Id', 1}, {'Result-Code', 1},
     {'Origin-Host', 1}, {'Origin-Realm', 1},
     {'User-Name', {0, 1}}, {'Origin-AAA-Protocol', {0, 1}},
     {'Origin-State-Id', {0, 1}}, {'Error-Message', {0, 1}},
     {'Error-Reporting-Host', {0, 1}},
     {'Failed-AVP', {0, '*'}}, {'Redirect-Host', {0, '*'}},
     {'Redirect-Host-Usage', {0, 1}},
     {'Redirect-Max-Cache-Time', {0, 1}},
     {'Service-Type', {0, 1}},
     {'Configuration-Token', {0, '*'}},
     {'Idle-Timeout', {0, 1}},
     {'Authorization-Lifetime', {0, 1}},
     {'Auth-Grace-Period', {0, 1}},
     {'Re-Auth-Request-Type', {0, 1}}, {'State', {0, 1}},
     {'Class', {0, '*'}}, {'Reply-Message', {0, '*'}},
     {'Prompt', {0, 1}}, {'Proxy-Info', {0, '*'}},
     {'AVP', {0, '*'}}];
avp_arity('STR') ->
    [{'Session-Id', 1}, {'Origin-Host', 1},
     {'Origin-Realm', 1}, {'Destination-Realm', 1},
     {'Auth-Application-Id', 1}, {'Termination-Cause', 1},
     {'User-Name', {0, 1}}, {'Destination-Host', {0, 1}},
     {'Class', {0, '*'}}, {'Origin-AAA-Protocol', {0, 1}},
     {'Origin-State-Id', {0, 1}}, {'Proxy-Info', {0, '*'}},
     {'Route-Record', {0, '*'}}, {'AVP', {0, '*'}}];
avp_arity('STA') ->
    [{'Session-Id', 1}, {'Result-Code', 1},
     {'Origin-Host', 1}, {'Origin-Realm', 1},
     {'User-Name', {0, 1}}, {'Class', {0, '*'}},
     {'Error-Message', {0, 1}},
     {'Error-Reporting-Host', {0, 1}},
     {'Failed-AVP', {0, '*'}},
     {'Origin-AAA-Protocol', {0, 1}},
     {'Origin-State-Id', {0, 1}},
     {'Redirect-Host', {0, '*'}},
     {'Redirect-Host-Usage', {0, 1}},
     {'Redirect-Max-Cache-Time', {0, 1}},
     {'Proxy-Info', {0, '*'}}, {'AVP', {0, '*'}}];
avp_arity('ASR') ->
    [{'Session-Id', 1}, {'Origin-Host', 1},
     {'Origin-Realm', 1}, {'Destination-Realm', 1},
     {'Destination-Host', 1}, {'Auth-Application-Id', 1},
     {'User-Name', {0, 1}}, {'Origin-AAA-Protocol', {0, 1}},
     {'Origin-State-Id', {0, 1}}, {'NAS-Identifier', {0, 1}},
     {'NAS-IP-Address', {0, 1}},
     {'NAS-IPv6-Address', {0, 1}}, {'NAS-Port', {0, 1}},
     {'NAS-Port-Id', {0, 1}}, {'NAS-Port-Type', {0, 1}},
     {'Service-Type', {0, 1}}, {'Framed-IP-Address', {0, 1}},
     {'Framed-IPv6-Prefix', {0, 1}},
     {'Framed-Interface-Id', {0, 1}},
     {'Called-Station-Id', {0, 1}},
     {'Calling-Station-Id', {0, 1}},
     {'Originating-Line-Info', {0, 1}},
     {'Acct-Session-Id', {0, 1}},
     {'Acct-Multi-Session-Id', {0, 1}}, {'State', {0, 1}},
     {'Class', {0, '*'}}, {'Reply-Message', {0, '*'}},
     {'Proxy-Info', {0, '*'}}, {'Route-Record', {0, '*'}},
     {'AVP', {0, '*'}}];
avp_arity('ASA') ->
    [{'Session-Id', 1}, {'Result-Code', 1},
     {'Origin-Host', 1}, {'Origin-Realm', 1},
     {'User-Name', {0, 1}}, {'Origin-AAA-Protocol', {0, 1}},
     {'Origin-State-Id', {0, 1}}, {'State', {0, 1}},
     {'Error-Message', {0, 1}},
     {'Error-Reporting-Host', {0, 1}},
     {'Failed-AVP', {0, '*'}}, {'Redirect-Host', {0, '*'}},
     {'Redirect-Host-Usage', {0, 1}},
     {'Redirect-Max-Cache-Time', {0, 1}},
     {'Proxy-Info', {0, '*'}}, {'AVP', {0, '*'}}];
avp_arity('ACR') ->
    [{'Session-Id', 1}, {'Origin-Host', 1},
     {'Origin-Realm', 1}, {'Destination-Realm', 1},
     {'Accounting-Record-Type', 1},
     {'Accounting-Record-Number', 1},
     {'Acct-Application-Id', 1}, {'User-Name', {0, 1}},
     {'Accounting-Sub-Session-Id', {0, 1}},
     {'Acct-Session-Id', {0, 1}},
     {'Acct-Multi-Session-Id', {0, 1}},
     {'Origin-AAA-Protocol', {0, 1}},
     {'Origin-State-Id', {0, 1}},
     {'Destination-Host', {0, 1}},
     {'Event-Timestamp', {0, 1}},
     {'Acct-Delay-Time', {0, 1}}, {'NAS-Identifier', {0, 1}},
     {'NAS-IP-Address', {0, 1}},
     {'NAS-IPv6-Address', {0, 1}}, {'NAS-Port', {0, 1}},
     {'NAS-Port-Id', {0, 1}}, {'NAS-Port-Type', {0, 1}},
     {'Class', {0, '*'}}, {'Service-Type', {0, 1}},
     {'Termination-Cause', {0, 1}},
     {'Accounting-Input-Octets', {0, 1}},
     {'Accounting-Input-Packets', {0, 1}},
     {'Accounting-Output-Octets', {0, 1}},
     {'Accounting-Output-Packets', {0, 1}},
     {'Acct-Authentic', {0, 1}},
     {'Accounting-Auth-Method', {0, 1}},
     {'Acct-Link-Count', {0, 1}},
     {'Acct-Session-Time', {0, 1}},
     {'Acct-Tunnel-Connection', {0, 1}},
     {'Acct-Tunnel-Packets-Lost', {0, 1}},
     {'Callback-Id', {0, 1}}, {'Callback-Number', {0, 1}},
     {'Called-Station-Id', {0, 1}},
     {'Calling-Station-Id', {0, 1}},
     {'Connect-Info', {0, '*'}},
     {'Originating-Line-Info', {0, 1}},
     {'Authorization-Lifetime', {0, 1}},
     {'Session-Timeout', {0, 1}}, {'Idle-Timeout', {0, 1}},
     {'Port-Limit', {0, 1}},
     {'Accounting-Realtime-Required', {0, 1}},
     {'Acct-Interim-Interval', {0, 1}},
     {'Filter-Id', {0, '*'}}, {'NAS-Filter-Rule', {0, '*'}},
     {'QoS-Filter-Rule', {0, '*'}},
     {'Framed-Appletalk-Link', {0, 1}},
     {'Framed-Appletalk-Network', {0, 1}},
     {'Framed-Appletalk-Zone', {0, 1}},
     {'Framed-Compression', {0, 1}},
     {'Framed-Interface-Id', {0, 1}},
     {'Framed-IP-Address', {0, 1}},
     {'Framed-IP-Netmask', {0, 1}},
     {'Framed-IPv6-Prefix', {0, '*'}},
     {'Framed-IPv6-Pool', {0, 1}},
     {'Framed-IPv6-Route', {0, '*'}},
     {'Framed-IPX-Network', {0, 1}}, {'Framed-MTU', {0, 1}},
     {'Framed-Pool', {0, 1}}, {'Framed-Protocol', {0, 1}},
     {'Framed-Route', {0, '*'}}, {'Framed-Routing', {0, 1}},
     {'Login-IP-Host', {0, '*'}},
     {'Login-IPv6-Host', {0, '*'}},
     {'Login-LAT-Group', {0, 1}}, {'Login-LAT-Node', {0, 1}},
     {'Login-LAT-Port', {0, 1}},
     {'Login-LAT-Service', {0, 1}},
     {'Login-Service', {0, 1}}, {'Login-TCP-Port', {0, 1}},
     {'Tunneling', {0, '*'}}, {'Proxy-Info', {0, '*'}},
     {'Route-Record', {0, '*'}}, {'AVP', {0, '*'}}];
avp_arity('ACA') ->
    [{'Session-Id', 1}, {'Result-Code', 1},
     {'Origin-Host', 1}, {'Origin-Realm', 1},
     {'Accounting-Record-Type', 1},
     {'Accounting-Record-Number', 1},
     {'Acct-Application-Id', 1}, {'User-Name', {0, 1}},
     {'Accounting-Sub-Session-Id', {0, 1}},
     {'Acct-Session-Id', {0, 1}},
     {'Acct-Multi-Session-Id', {0, 1}},
     {'Event-Timestamp', {0, 1}}, {'Error-Message', {0, 1}},
     {'Error-Reporting-Host', {0, 1}},
     {'Failed-AVP', {0, '*'}},
     {'Origin-AAA-Protocol', {0, 1}},
     {'Origin-State-Id', {0, 1}}, {'NAS-Identifier', {0, 1}},
     {'NAS-IP-Address', {0, 1}},
     {'NAS-IPv6-Address', {0, 1}}, {'NAS-Port', {0, 1}},
     {'NAS-Port-Id', {0, 1}}, {'NAS-Port-Type', {0, 1}},
     {'Service-Type', {0, 1}}, {'Termination-Cause', {0, 1}},
     {'Accounting-Realtime-Required', {0, 1}},
     {'Acct-Interim-Interval', {0, 1}}, {'Class', {0, '*'}},
     {'Proxy-Info', {0, '*'}}, {'AVP', {0, '*'}}];
avp_arity('CHAP-Auth') ->
    [{'CHAP-Algorithm', 1}, {'CHAP-Ident', 1},
     {'CHAP-Response', {0, 1}}, {'AVP', {0, '*'}}];
avp_arity('Tunneling') ->
    [{'Tunnel-Type', 1}, {'Tunnel-Medium-Type', 1},
     {'Tunnel-Client-Endpoint', 1},
     {'Tunnel-Server-Endpoint', 1},
     {'Tunnel-Preference', {0, 1}},
     {'Tunnel-Client-Auth-Id', {0, 1}},
     {'Tunnel-Server-Auth-Id', {0, 1}},
     {'Tunnel-Assignment-Id', {0, 1}},
     {'Tunnel-Password', {0, 1}},
     {'Tunnel-Private-Group-Id', {0, 1}}];
avp_arity('Proxy-Info') ->
    [{'Proxy-Host', 1}, {'Proxy-State', 1},
     {'AVP', {0, '*'}}];
avp_arity('Failed-AVP') -> [{'AVP', {1, '*'}}];
avp_arity('Experimental-Result') ->
    [{'Vendor-Id', 1}, {'Experimental-Result-Code', 1}];
avp_arity('Vendor-Specific-Application-Id') ->
    [{'Vendor-Id', 1}, {'Auth-Application-Id', {0, 1}},
     {'Acct-Application-Id', {0, 1}}];
avp_arity(_) -> erlang:error(badarg).

avp_arity('AAR', 'Session-Id') -> 1;
avp_arity('AAR', 'Auth-Application-Id') -> 1;
avp_arity('AAR', 'Origin-Host') -> 1;
avp_arity('AAR', 'Origin-Realm') -> 1;
avp_arity('AAR', 'Destination-Realm') -> 1;
avp_arity('AAR', 'Auth-Request-Type') -> 1;
avp_arity('AAR', 'Destination-Host') -> {0, 1};
avp_arity('AAR', 'NAS-Identifier') -> {0, 1};
avp_arity('AAR', 'NAS-IP-Address') -> {0, 1};
avp_arity('AAR', 'NAS-IPv6-Address') -> {0, 1};
avp_arity('AAR', 'NAS-Port') -> {0, 1};
avp_arity('AAR', 'NAS-Port-Id') -> {0, 1};
avp_arity('AAR', 'NAS-Port-Type') -> {0, 1};
avp_arity('AAR', 'Origin-AAA-Protocol') -> {0, 1};
avp_arity('AAR', 'Origin-State-Id') -> {0, 1};
avp_arity('AAR', 'Port-Limit') -> {0, 1};
avp_arity('AAR', 'User-Name') -> {0, 1};
avp_arity('AAR', 'User-Password') -> {0, 1};
avp_arity('AAR', 'Service-Type') -> {0, 1};
avp_arity('AAR', 'State') -> {0, 1};
avp_arity('AAR', 'Authorization-Lifetime') -> {0, 1};
avp_arity('AAR', 'Auth-Grace-Period') -> {0, 1};
avp_arity('AAR', 'Auth-Session-State') -> {0, 1};
avp_arity('AAR', 'Callback-Number') -> {0, 1};
avp_arity('AAR', 'Called-Station-Id') -> {0, 1};
avp_arity('AAR', 'Calling-Station-Id') -> {0, 1};
avp_arity('AAR', 'Originating-Line-Info') -> {0, 1};
avp_arity('AAR', 'Connect-Info') -> {0, 1};
avp_arity('AAR', 'CHAP-Auth') -> {0, 1};
avp_arity('AAR', 'CHAP-Challenge') -> {0, 1};
avp_arity('AAR', 'Framed-Compression') -> {0, '*'};
avp_arity('AAR', 'Framed-Interface-Id') -> {0, 1};
avp_arity('AAR', 'Framed-IP-Address') -> {0, 1};
avp_arity('AAR', 'Framed-IPv6-Prefix') -> {0, '*'};
avp_arity('AAR', 'Framed-IP-Netmask') -> {0, 1};
avp_arity('AAR', 'Framed-MTU') -> {0, 1};
avp_arity('AAR', 'Framed-Protocol') -> {0, 1};
avp_arity('AAR', 'ARAP-Password') -> {0, 1};
avp_arity('AAR', 'ARAP-Security') -> {0, 1};
avp_arity('AAR', 'ARAP-Security-Data') -> {0, '*'};
avp_arity('AAR', 'Login-IP-Host') -> {0, '*'};
avp_arity('AAR', 'Login-IPv6-Host') -> {0, '*'};
avp_arity('AAR', 'Login-LAT-Group') -> {0, 1};
avp_arity('AAR', 'Login-LAT-Node') -> {0, 1};
avp_arity('AAR', 'Login-LAT-Port') -> {0, 1};
avp_arity('AAR', 'Login-LAT-Service') -> {0, 1};
avp_arity('AAR', 'Tunneling') -> {0, '*'};
avp_arity('AAR', 'Proxy-Info') -> {0, '*'};
avp_arity('AAR', 'Route-Record') -> {0, '*'};
avp_arity('AAR', 'AVP') -> {0, '*'};
avp_arity('AAA', 'Session-Id') -> 1;
avp_arity('AAA', 'Auth-Application-Id') -> 1;
avp_arity('AAA', 'Auth-Request-Type') -> 1;
avp_arity('AAA', 'Result-Code') -> 1;
avp_arity('AAA', 'Origin-Host') -> 1;
avp_arity('AAA', 'Origin-Realm') -> 1;
avp_arity('AAA', 'User-Name') -> {0, 1};
avp_arity('AAA', 'Service-Type') -> {0, 1};
avp_arity('AAA', 'Class') -> {0, '*'};
avp_arity('AAA', 'Configuration-Token') -> {0, '*'};
avp_arity('AAA', 'Acct-Interim-Interval') -> {0, 1};
avp_arity('AAA', 'Error-Message') -> {0, 1};
avp_arity('AAA', 'Error-Reporting-Host') -> {0, 1};
avp_arity('AAA', 'Failed-AVP') -> {0, '*'};
avp_arity('AAA', 'Idle-Timeout') -> {0, 1};
avp_arity('AAA', 'Authorization-Lifetime') -> {0, 1};
avp_arity('AAA', 'Auth-Grace-Period') -> {0, 1};
avp_arity('AAA', 'Auth-Session-State') -> {0, 1};
avp_arity('AAA', 'Re-Auth-Request-Type') -> {0, 1};
avp_arity('AAA', 'Multi-Round-Time-Out') -> {0, 1};
avp_arity('AAA', 'Session-Timeout') -> {0, 1};
avp_arity('AAA', 'State') -> {0, 1};
avp_arity('AAA', 'Reply-Message') -> {0, '*'};
avp_arity('AAA', 'Origin-AAA-Protocol') -> {0, 1};
avp_arity('AAA', 'Origin-State-Id') -> {0, 1};
avp_arity('AAA', 'Filter-Id') -> {0, '*'};
avp_arity('AAA', 'Password-Retry') -> {0, 1};
avp_arity('AAA', 'Port-Limit') -> {0, 1};
avp_arity('AAA', 'Prompt') -> {0, 1};
avp_arity('AAA', 'ARAP-Challenge-Response') -> {0, 1};
avp_arity('AAA', 'ARAP-Features') -> {0, 1};
avp_arity('AAA', 'ARAP-Security') -> {0, 1};
avp_arity('AAA', 'ARAP-Security-Data') -> {0, '*'};
avp_arity('AAA', 'ARAP-Zone-Access') -> {0, 1};
avp_arity('AAA', 'Callback-Id') -> {0, 1};
avp_arity('AAA', 'Callback-Number') -> {0, 1};
avp_arity('AAA', 'Framed-Appletalk-Link') -> {0, 1};
avp_arity('AAA', 'Framed-Appletalk-Network') ->
    {0, '*'};
avp_arity('AAA', 'Framed-Appletalk-Zone') -> {0, 1};
avp_arity('AAA', 'Framed-Compression') -> {0, '*'};
avp_arity('AAA', 'Framed-Interface-Id') -> {0, 1};
avp_arity('AAA', 'Framed-IP-Address') -> {0, 1};
avp_arity('AAA', 'Framed-IPv6-Prefix') -> {0, '*'};
avp_arity('AAA', 'Framed-IPv6-Pool') -> {0, 1};
avp_arity('AAA', 'Framed-IPv6-Route') -> {0, '*'};
avp_arity('AAA', 'Framed-IP-Netmask') -> {0, 1};
avp_arity('AAA', 'Framed-Route') -> {0, '*'};
avp_arity('AAA', 'Framed-Pool') -> {0, 1};
avp_arity('AAA', 'Framed-IPX-Network') -> {0, 1};
avp_arity('AAA', 'Framed-MTU') -> {0, 1};
avp_arity('AAA', 'Framed-Protocol') -> {0, 1};
avp_arity('AAA', 'Framed-Routing') -> {0, 1};
avp_arity('AAA', 'Login-IP-Host') -> {0, '*'};
avp_arity('AAA', 'Login-IPv6-Host') -> {0, '*'};
avp_arity('AAA', 'Login-LAT-Group') -> {0, 1};
avp_arity('AAA', 'Login-LAT-Node') -> {0, 1};
avp_arity('AAA', 'Login-LAT-Port') -> {0, 1};
avp_arity('AAA', 'Login-LAT-Service') -> {0, 1};
avp_arity('AAA', 'Login-Service') -> {0, 1};
avp_arity('AAA', 'Login-TCP-Port') -> {0, 1};
avp_arity('AAA', 'NAS-Filter-Rule') -> {0, '*'};
avp_arity('AAA', 'QoS-Filter-Rule') -> {0, '*'};
avp_arity('AAA', 'Tunneling') -> {0, '*'};
avp_arity('AAA', 'Redirect-Host') -> {0, '*'};
avp_arity('AAA', 'Redirect-Host-Usage') -> {0, 1};
avp_arity('AAA', 'Redirect-Max-Cache-Time') -> {0, 1};
avp_arity('AAA', 'Proxy-Info') -> {0, '*'};
avp_arity('AAA', 'AVP') -> {0, '*'};
avp_arity('RAR', 'Session-Id') -> 1;
avp_arity('RAR', 'Origin-Host') -> 1;
avp_arity('RAR', 'Origin-Realm') -> 1;
avp_arity('RAR', 'Destination-Realm') -> 1;
avp_arity('RAR', 'Destination-Host') -> 1;
avp_arity('RAR', 'Auth-Application-Id') -> 1;
avp_arity('RAR', 'Re-Auth-Request-Type') -> 1;
avp_arity('RAR', 'User-Name') -> {0, 1};
avp_arity('RAR', 'Origin-AAA-Protocol') -> {0, 1};
avp_arity('RAR', 'Origin-State-Id') -> {0, 1};
avp_arity('RAR', 'NAS-Identifier') -> {0, 1};
avp_arity('RAR', 'NAS-IP-Address') -> {0, 1};
avp_arity('RAR', 'NAS-IPv6-Address') -> {0, 1};
avp_arity('RAR', 'NAS-Port') -> {0, 1};
avp_arity('RAR', 'NAS-Port-Id') -> {0, 1};
avp_arity('RAR', 'NAS-Port-Type') -> {0, 1};
avp_arity('RAR', 'Service-Type') -> {0, 1};
avp_arity('RAR', 'Framed-IP-Address') -> {0, 1};
avp_arity('RAR', 'Framed-IPv6-Prefix') -> {0, 1};
avp_arity('RAR', 'Framed-Interface-Id') -> {0, 1};
avp_arity('RAR', 'Called-Station-Id') -> {0, 1};
avp_arity('RAR', 'Calling-Station-Id') -> {0, 1};
avp_arity('RAR', 'Originating-Line-Info') -> {0, 1};
avp_arity('RAR', 'Acct-Session-Id') -> {0, 1};
avp_arity('RAR', 'Acct-Multi-Session-Id') -> {0, 1};
avp_arity('RAR', 'State') -> {0, 1};
avp_arity('RAR', 'Class') -> {0, '*'};
avp_arity('RAR', 'Reply-Message') -> {0, 1};
avp_arity('RAR', 'Proxy-Info') -> {0, '*'};
avp_arity('RAR', 'Route-Record') -> {0, '*'};
avp_arity('RAR', 'AVP') -> {0, '*'};
avp_arity('RAA', 'Session-Id') -> 1;
avp_arity('RAA', 'Result-Code') -> 1;
avp_arity('RAA', 'Origin-Host') -> 1;
avp_arity('RAA', 'Origin-Realm') -> 1;
avp_arity('RAA', 'User-Name') -> {0, 1};
avp_arity('RAA', 'Origin-AAA-Protocol') -> {0, 1};
avp_arity('RAA', 'Origin-State-Id') -> {0, 1};
avp_arity('RAA', 'Error-Message') -> {0, 1};
avp_arity('RAA', 'Error-Reporting-Host') -> {0, 1};
avp_arity('RAA', 'Failed-AVP') -> {0, '*'};
avp_arity('RAA', 'Redirect-Host') -> {0, '*'};
avp_arity('RAA', 'Redirect-Host-Usage') -> {0, 1};
avp_arity('RAA', 'Redirect-Max-Cache-Time') -> {0, 1};
avp_arity('RAA', 'Service-Type') -> {0, 1};
avp_arity('RAA', 'Configuration-Token') -> {0, '*'};
avp_arity('RAA', 'Idle-Timeout') -> {0, 1};
avp_arity('RAA', 'Authorization-Lifetime') -> {0, 1};
avp_arity('RAA', 'Auth-Grace-Period') -> {0, 1};
avp_arity('RAA', 'Re-Auth-Request-Type') -> {0, 1};
avp_arity('RAA', 'State') -> {0, 1};
avp_arity('RAA', 'Class') -> {0, '*'};
avp_arity('RAA', 'Reply-Message') -> {0, '*'};
avp_arity('RAA', 'Prompt') -> {0, 1};
avp_arity('RAA', 'Proxy-Info') -> {0, '*'};
avp_arity('RAA', 'AVP') -> {0, '*'};
avp_arity('STR', 'Session-Id') -> 1;
avp_arity('STR', 'Origin-Host') -> 1;
avp_arity('STR', 'Origin-Realm') -> 1;
avp_arity('STR', 'Destination-Realm') -> 1;
avp_arity('STR', 'Auth-Application-Id') -> 1;
avp_arity('STR', 'Termination-Cause') -> 1;
avp_arity('STR', 'User-Name') -> {0, 1};
avp_arity('STR', 'Destination-Host') -> {0, 1};
avp_arity('STR', 'Class') -> {0, '*'};
avp_arity('STR', 'Origin-AAA-Protocol') -> {0, 1};
avp_arity('STR', 'Origin-State-Id') -> {0, 1};
avp_arity('STR', 'Proxy-Info') -> {0, '*'};
avp_arity('STR', 'Route-Record') -> {0, '*'};
avp_arity('STR', 'AVP') -> {0, '*'};
avp_arity('STA', 'Session-Id') -> 1;
avp_arity('STA', 'Result-Code') -> 1;
avp_arity('STA', 'Origin-Host') -> 1;
avp_arity('STA', 'Origin-Realm') -> 1;
avp_arity('STA', 'User-Name') -> {0, 1};
avp_arity('STA', 'Class') -> {0, '*'};
avp_arity('STA', 'Error-Message') -> {0, 1};
avp_arity('STA', 'Error-Reporting-Host') -> {0, 1};
avp_arity('STA', 'Failed-AVP') -> {0, '*'};
avp_arity('STA', 'Origin-AAA-Protocol') -> {0, 1};
avp_arity('STA', 'Origin-State-Id') -> {0, 1};
avp_arity('STA', 'Redirect-Host') -> {0, '*'};
avp_arity('STA', 'Redirect-Host-Usage') -> {0, 1};
avp_arity('STA', 'Redirect-Max-Cache-Time') -> {0, 1};
avp_arity('STA', 'Proxy-Info') -> {0, '*'};
avp_arity('STA', 'AVP') -> {0, '*'};
avp_arity('ASR', 'Session-Id') -> 1;
avp_arity('ASR', 'Origin-Host') -> 1;
avp_arity('ASR', 'Origin-Realm') -> 1;
avp_arity('ASR', 'Destination-Realm') -> 1;
avp_arity('ASR', 'Destination-Host') -> 1;
avp_arity('ASR', 'Auth-Application-Id') -> 1;
avp_arity('ASR', 'User-Name') -> {0, 1};
avp_arity('ASR', 'Origin-AAA-Protocol') -> {0, 1};
avp_arity('ASR', 'Origin-State-Id') -> {0, 1};
avp_arity('ASR', 'NAS-Identifier') -> {0, 1};
avp_arity('ASR', 'NAS-IP-Address') -> {0, 1};
avp_arity('ASR', 'NAS-IPv6-Address') -> {0, 1};
avp_arity('ASR', 'NAS-Port') -> {0, 1};
avp_arity('ASR', 'NAS-Port-Id') -> {0, 1};
avp_arity('ASR', 'NAS-Port-Type') -> {0, 1};
avp_arity('ASR', 'Service-Type') -> {0, 1};
avp_arity('ASR', 'Framed-IP-Address') -> {0, 1};
avp_arity('ASR', 'Framed-IPv6-Prefix') -> {0, 1};
avp_arity('ASR', 'Framed-Interface-Id') -> {0, 1};
avp_arity('ASR', 'Called-Station-Id') -> {0, 1};
avp_arity('ASR', 'Calling-Station-Id') -> {0, 1};
avp_arity('ASR', 'Originating-Line-Info') -> {0, 1};
avp_arity('ASR', 'Acct-Session-Id') -> {0, 1};
avp_arity('ASR', 'Acct-Multi-Session-Id') -> {0, 1};
avp_arity('ASR', 'State') -> {0, 1};
avp_arity('ASR', 'Class') -> {0, '*'};
avp_arity('ASR', 'Reply-Message') -> {0, '*'};
avp_arity('ASR', 'Proxy-Info') -> {0, '*'};
avp_arity('ASR', 'Route-Record') -> {0, '*'};
avp_arity('ASR', 'AVP') -> {0, '*'};
avp_arity('ASA', 'Session-Id') -> 1;
avp_arity('ASA', 'Result-Code') -> 1;
avp_arity('ASA', 'Origin-Host') -> 1;
avp_arity('ASA', 'Origin-Realm') -> 1;
avp_arity('ASA', 'User-Name') -> {0, 1};
avp_arity('ASA', 'Origin-AAA-Protocol') -> {0, 1};
avp_arity('ASA', 'Origin-State-Id') -> {0, 1};
avp_arity('ASA', 'State') -> {0, 1};
avp_arity('ASA', 'Error-Message') -> {0, 1};
avp_arity('ASA', 'Error-Reporting-Host') -> {0, 1};
avp_arity('ASA', 'Failed-AVP') -> {0, '*'};
avp_arity('ASA', 'Redirect-Host') -> {0, '*'};
avp_arity('ASA', 'Redirect-Host-Usage') -> {0, 1};
avp_arity('ASA', 'Redirect-Max-Cache-Time') -> {0, 1};
avp_arity('ASA', 'Proxy-Info') -> {0, '*'};
avp_arity('ASA', 'AVP') -> {0, '*'};
avp_arity('ACR', 'Session-Id') -> 1;
avp_arity('ACR', 'Origin-Host') -> 1;
avp_arity('ACR', 'Origin-Realm') -> 1;
avp_arity('ACR', 'Destination-Realm') -> 1;
avp_arity('ACR', 'Accounting-Record-Type') -> 1;
avp_arity('ACR', 'Accounting-Record-Number') -> 1;
avp_arity('ACR', 'Acct-Application-Id') -> 1;
avp_arity('ACR', 'User-Name') -> {0, 1};
avp_arity('ACR', 'Accounting-Sub-Session-Id') -> {0, 1};
avp_arity('ACR', 'Acct-Session-Id') -> {0, 1};
avp_arity('ACR', 'Acct-Multi-Session-Id') -> {0, 1};
avp_arity('ACR', 'Origin-AAA-Protocol') -> {0, 1};
avp_arity('ACR', 'Origin-State-Id') -> {0, 1};
avp_arity('ACR', 'Destination-Host') -> {0, 1};
avp_arity('ACR', 'Event-Timestamp') -> {0, 1};
avp_arity('ACR', 'Acct-Delay-Time') -> {0, 1};
avp_arity('ACR', 'NAS-Identifier') -> {0, 1};
avp_arity('ACR', 'NAS-IP-Address') -> {0, 1};
avp_arity('ACR', 'NAS-IPv6-Address') -> {0, 1};
avp_arity('ACR', 'NAS-Port') -> {0, 1};
avp_arity('ACR', 'NAS-Port-Id') -> {0, 1};
avp_arity('ACR', 'NAS-Port-Type') -> {0, 1};
avp_arity('ACR', 'Class') -> {0, '*'};
avp_arity('ACR', 'Service-Type') -> {0, 1};
avp_arity('ACR', 'Termination-Cause') -> {0, 1};
avp_arity('ACR', 'Accounting-Input-Octets') -> {0, 1};
avp_arity('ACR', 'Accounting-Input-Packets') -> {0, 1};
avp_arity('ACR', 'Accounting-Output-Octets') -> {0, 1};
avp_arity('ACR', 'Accounting-Output-Packets') -> {0, 1};
avp_arity('ACR', 'Acct-Authentic') -> {0, 1};
avp_arity('ACR', 'Accounting-Auth-Method') -> {0, 1};
avp_arity('ACR', 'Acct-Link-Count') -> {0, 1};
avp_arity('ACR', 'Acct-Session-Time') -> {0, 1};
avp_arity('ACR', 'Acct-Tunnel-Connection') -> {0, 1};
avp_arity('ACR', 'Acct-Tunnel-Packets-Lost') -> {0, 1};
avp_arity('ACR', 'Callback-Id') -> {0, 1};
avp_arity('ACR', 'Callback-Number') -> {0, 1};
avp_arity('ACR', 'Called-Station-Id') -> {0, 1};
avp_arity('ACR', 'Calling-Station-Id') -> {0, 1};
avp_arity('ACR', 'Connect-Info') -> {0, '*'};
avp_arity('ACR', 'Originating-Line-Info') -> {0, 1};
avp_arity('ACR', 'Authorization-Lifetime') -> {0, 1};
avp_arity('ACR', 'Session-Timeout') -> {0, 1};
avp_arity('ACR', 'Idle-Timeout') -> {0, 1};
avp_arity('ACR', 'Port-Limit') -> {0, 1};
avp_arity('ACR', 'Accounting-Realtime-Required') ->
    {0, 1};
avp_arity('ACR', 'Acct-Interim-Interval') -> {0, 1};
avp_arity('ACR', 'Filter-Id') -> {0, '*'};
avp_arity('ACR', 'NAS-Filter-Rule') -> {0, '*'};
avp_arity('ACR', 'QoS-Filter-Rule') -> {0, '*'};
avp_arity('ACR', 'Framed-Appletalk-Link') -> {0, 1};
avp_arity('ACR', 'Framed-Appletalk-Network') -> {0, 1};
avp_arity('ACR', 'Framed-Appletalk-Zone') -> {0, 1};
avp_arity('ACR', 'Framed-Compression') -> {0, 1};
avp_arity('ACR', 'Framed-Interface-Id') -> {0, 1};
avp_arity('ACR', 'Framed-IP-Address') -> {0, 1};
avp_arity('ACR', 'Framed-IP-Netmask') -> {0, 1};
avp_arity('ACR', 'Framed-IPv6-Prefix') -> {0, '*'};
avp_arity('ACR', 'Framed-IPv6-Pool') -> {0, 1};
avp_arity('ACR', 'Framed-IPv6-Route') -> {0, '*'};
avp_arity('ACR', 'Framed-IPX-Network') -> {0, 1};
avp_arity('ACR', 'Framed-MTU') -> {0, 1};
avp_arity('ACR', 'Framed-Pool') -> {0, 1};
avp_arity('ACR', 'Framed-Protocol') -> {0, 1};
avp_arity('ACR', 'Framed-Route') -> {0, '*'};
avp_arity('ACR', 'Framed-Routing') -> {0, 1};
avp_arity('ACR', 'Login-IP-Host') -> {0, '*'};
avp_arity('ACR', 'Login-IPv6-Host') -> {0, '*'};
avp_arity('ACR', 'Login-LAT-Group') -> {0, 1};
avp_arity('ACR', 'Login-LAT-Node') -> {0, 1};
avp_arity('ACR', 'Login-LAT-Port') -> {0, 1};
avp_arity('ACR', 'Login-LAT-Service') -> {0, 1};
avp_arity('ACR', 'Login-Service') -> {0, 1};
avp_arity('ACR', 'Login-TCP-Port') -> {0, 1};
avp_arity('ACR', 'Tunneling') -> {0, '*'};
avp_arity('ACR', 'Proxy-Info') -> {0, '*'};
avp_arity('ACR', 'Route-Record') -> {0, '*'};
avp_arity('ACR', 'AVP') -> {0, '*'};
avp_arity('ACA', 'Session-Id') -> 1;
avp_arity('ACA', 'Result-Code') -> 1;
avp_arity('ACA', 'Origin-Host') -> 1;
avp_arity('ACA', 'Origin-Realm') -> 1;
avp_arity('ACA', 'Accounting-Record-Type') -> 1;
avp_arity('ACA', 'Accounting-Record-Number') -> 1;
avp_arity('ACA', 'Acct-Application-Id') -> 1;
avp_arity('ACA', 'User-Name') -> {0, 1};
avp_arity('ACA', 'Accounting-Sub-Session-Id') -> {0, 1};
avp_arity('ACA', 'Acct-Session-Id') -> {0, 1};
avp_arity('ACA', 'Acct-Multi-Session-Id') -> {0, 1};
avp_arity('ACA', 'Event-Timestamp') -> {0, 1};
avp_arity('ACA', 'Error-Message') -> {0, 1};
avp_arity('ACA', 'Error-Reporting-Host') -> {0, 1};
avp_arity('ACA', 'Failed-AVP') -> {0, '*'};
avp_arity('ACA', 'Origin-AAA-Protocol') -> {0, 1};
avp_arity('ACA', 'Origin-State-Id') -> {0, 1};
avp_arity('ACA', 'NAS-Identifier') -> {0, 1};
avp_arity('ACA', 'NAS-IP-Address') -> {0, 1};
avp_arity('ACA', 'NAS-IPv6-Address') -> {0, 1};
avp_arity('ACA', 'NAS-Port') -> {0, 1};
avp_arity('ACA', 'NAS-Port-Id') -> {0, 1};
avp_arity('ACA', 'NAS-Port-Type') -> {0, 1};
avp_arity('ACA', 'Service-Type') -> {0, 1};
avp_arity('ACA', 'Termination-Cause') -> {0, 1};
avp_arity('ACA', 'Accounting-Realtime-Required') ->
    {0, 1};
avp_arity('ACA', 'Acct-Interim-Interval') -> {0, 1};
avp_arity('ACA', 'Class') -> {0, '*'};
avp_arity('ACA', 'Proxy-Info') -> {0, '*'};
avp_arity('ACA', 'AVP') -> {0, '*'};
avp_arity('CHAP-Auth', 'CHAP-Algorithm') -> 1;
avp_arity('CHAP-Auth', 'CHAP-Ident') -> 1;
avp_arity('CHAP-Auth', 'CHAP-Response') -> {0, 1};
avp_arity('CHAP-Auth', 'AVP') -> {0, '*'};
avp_arity('Tunneling', 'Tunnel-Type') -> 1;
avp_arity('Tunneling', 'Tunnel-Medium-Type') -> 1;
avp_arity('Tunneling', 'Tunnel-Client-Endpoint') -> 1;
avp_arity('Tunneling', 'Tunnel-Server-Endpoint') -> 1;
avp_arity('Tunneling', 'Tunnel-Preference') -> {0, 1};
avp_arity('Tunneling', 'Tunnel-Client-Auth-Id') ->
    {0, 1};
avp_arity('Tunneling', 'Tunnel-Server-Auth-Id') ->
    {0, 1};
avp_arity('Tunneling', 'Tunnel-Assignment-Id') ->
    {0, 1};
avp_arity('Tunneling', 'Tunnel-Password') -> {0, 1};
avp_arity('Tunneling', 'Tunnel-Private-Group-Id') ->
    {0, 1};
avp_arity('Proxy-Info', 'Proxy-Host') -> 1;
avp_arity('Proxy-Info', 'Proxy-State') -> 1;
avp_arity('Proxy-Info', 'AVP') -> {0, '*'};
avp_arity('Failed-AVP', 'AVP') -> {1, '*'};
avp_arity('Experimental-Result', 'Vendor-Id') -> 1;
avp_arity('Experimental-Result',
	  'Experimental-Result-Code') ->
    1;
avp_arity('Vendor-Specific-Application-Id',
	  'Vendor-Id') ->
    1;
avp_arity('Vendor-Specific-Application-Id',
	  'Auth-Application-Id') ->
    {0, 1};
avp_arity('Vendor-Specific-Application-Id',
	  'Acct-Application-Id') ->
    {0, 1};
avp_arity(_, _) -> 0.

avp_header('ARAP-Challenge-Response') -> {84, 192, 0};
avp_header('ARAP-Features') -> {71, 192, 0};
avp_header('ARAP-Password') -> {70, 192, 0};
avp_header('ARAP-Security') -> {73, 192, 0};
avp_header('ARAP-Security-Data') -> {74, 192, 0};
avp_header('ARAP-Zone-Access') -> {72, 192, 0};
avp_header('Accounting-Auth-Method') -> {406, 192, 0};
avp_header('Accounting-Input-Octets') -> {363, 192, 0};
avp_header('Accounting-Input-Packets') -> {365, 192, 0};
avp_header('Accounting-Output-Octets') -> {364, 192, 0};
avp_header('Accounting-Output-Packets') ->
    {366, 192, 0};
avp_header('Acct-Authentic') -> {45, 192, 0};
avp_header('Acct-Delay-Time') -> {41, 192, 0};
avp_header('Acct-Link-Count') -> {51, 192, 0};
avp_header('Acct-Session-Time') -> {46, 192, 0};
avp_header('Acct-Tunnel-Connection') -> {68, 192, 0};
avp_header('Acct-Tunnel-Packets-Lost') -> {86, 192, 0};
avp_header('CHAP-Algorithm') -> {403, 192, 0};
avp_header('CHAP-Auth') -> {402, 192, 0};
avp_header('CHAP-Challenge') -> {60, 192, 0};
avp_header('CHAP-Ident') -> {404, 192, 0};
avp_header('CHAP-Response') -> {405, 192, 0};
avp_header('Callback-Id') -> {20, 192, 0};
avp_header('Callback-Number') -> {19, 192, 0};
avp_header('Called-Station-Id') -> {30, 192, 0};
avp_header('Calling-Station-Id') -> {31, 192, 0};
avp_header('Configuration-Token') -> {78, 192, 0};
avp_header('Connect-Info') -> {77, 192, 0};
avp_header('Filter-Id') -> {11, 192, 0};
avp_header('Framed-Appletalk-Link') -> {37, 192, 0};
avp_header('Framed-Appletalk-Network') -> {38, 192, 0};
avp_header('Framed-Appletalk-Zone') ->
    {39, 64, undefined};
avp_header('Framed-Compression') -> {13, 192, 0};
avp_header('Framed-IP-Address') -> {8, 192, 0};
avp_header('Framed-IP-Netmask') -> {9, 192, 0};
avp_header('Framed-IPX-Network') -> {23, 192, 0};
avp_header('Framed-IPv6-Pool') -> {100, 192, 0};
avp_header('Framed-IPv6-Prefix') -> {97, 192, 0};
avp_header('Framed-IPv6-Route') -> {99, 192, 0};
avp_header('Framed-Interface-Id') -> {96, 192, 0};
avp_header('Framed-MTU') -> {12, 192, 0};
avp_header('Framed-Pool') -> {88, 192, 0};
avp_header('Framed-Protocol') -> {7, 192, 0};
avp_header('Framed-Route') -> {22, 192, 0};
avp_header('Framed-Routing') -> {10, 192, 0};
avp_header('Idle-Timeout') -> {28, 192, 0};
avp_header('Login-IP-Host') -> {14, 192, 0};
avp_header('Login-IPv6-Host') -> {98, 192, 0};
avp_header('Login-LAT-Group') -> {36, 192, 0};
avp_header('Login-LAT-Node') -> {35, 192, 0};
avp_header('Login-LAT-Port') -> {63, 192, 0};
avp_header('Login-LAT-Service') -> {34, 192, 0};
avp_header('Login-Service') -> {15, 192, 0};
avp_header('Login-TCP-Port') -> {16, 192, 0};
avp_header('NAS-Filter-Rule') -> {400, 192, 0};
avp_header('NAS-IP-Address') -> {4, 64, undefined};
avp_header('NAS-IPv6-Address') -> {95, 64, undefined};
avp_header('NAS-Identifier') -> {32, 64, undefined};
avp_header('NAS-Port') -> {5, 192, 0};
avp_header('NAS-Port-Id') -> {87, 192, 0};
avp_header('NAS-Port-Type') -> {61, 192, 0};
avp_header('Origin-AAA-Protocol') ->
    {408, 64, undefined};
avp_header('Originating-Line-Info') -> {94, 192, 0};
avp_header('Password-Retry') -> {75, 192, 0};
avp_header('Port-Limit') -> {62, 192, 0};
avp_header('Prompt') -> {76, 192, 0};
avp_header('QoS-Filter-Rule') -> {407, 0, undefined};
avp_header('Reply-Message') -> {18, 192, 0};
avp_header('Service-Type') -> {6, 192, 0};
avp_header('State') -> {24, 64, undefined};
avp_header('Tunnel-Assignment-Id') -> {82, 192, 0};
avp_header('Tunnel-Client-Auth-Id') -> {90, 192, 0};
avp_header('Tunnel-Client-Endpoint') -> {66, 192, 0};
avp_header('Tunnel-Medium-Type') -> {65, 192, 0};
avp_header('Tunnel-Password') -> {69, 192, 0};
avp_header('Tunnel-Preference') -> {83, 192, 0};
avp_header('Tunnel-Private-Group-Id') -> {81, 192, 0};
avp_header('Tunnel-Server-Auth-Id') -> {91, 192, 0};
avp_header('Tunnel-Server-Endpoint') -> {67, 192, 0};
avp_header('Tunnel-Type') -> {64, 192, 0};
avp_header('Tunneling') -> {401, 192, 0};
avp_header('User-Password') -> {2, 192, 0};
avp_header('Accounting-Realtime-Required') ->
    diameter_gen_base_rfc6733:avp_header('Accounting-Realtime-Required');
avp_header('Accounting-Record-Number') ->
    diameter_gen_base_rfc6733:avp_header('Accounting-Record-Number');
avp_header('Accounting-Record-Type') ->
    diameter_gen_base_rfc6733:avp_header('Accounting-Record-Type');
avp_header('Accounting-Sub-Session-Id') ->
    diameter_gen_base_rfc6733:avp_header('Accounting-Sub-Session-Id');
avp_header('Acct-Application-Id') ->
    diameter_gen_base_rfc6733:avp_header('Acct-Application-Id');
avp_header('Acct-Interim-Interval') ->
    diameter_gen_base_rfc6733:avp_header('Acct-Interim-Interval');
avp_header('Acct-Multi-Session-Id') ->
    diameter_gen_base_rfc6733:avp_header('Acct-Multi-Session-Id');
avp_header('Acct-Session-Id') ->
    diameter_gen_base_rfc6733:avp_header('Acct-Session-Id');
avp_header('Auth-Application-Id') ->
    diameter_gen_base_rfc6733:avp_header('Auth-Application-Id');
avp_header('Auth-Grace-Period') ->
    diameter_gen_base_rfc6733:avp_header('Auth-Grace-Period');
avp_header('Auth-Request-Type') ->
    diameter_gen_base_rfc6733:avp_header('Auth-Request-Type');
avp_header('Auth-Session-State') ->
    diameter_gen_base_rfc6733:avp_header('Auth-Session-State');
avp_header('Authorization-Lifetime') ->
    diameter_gen_base_rfc6733:avp_header('Authorization-Lifetime');
avp_header('Class') ->
    diameter_gen_base_rfc6733:avp_header('Class');
avp_header('Destination-Host') ->
    diameter_gen_base_rfc6733:avp_header('Destination-Host');
avp_header('Destination-Realm') ->
    diameter_gen_base_rfc6733:avp_header('Destination-Realm');
avp_header('Disconnect-Cause') ->
    diameter_gen_base_rfc6733:avp_header('Disconnect-Cause');
avp_header('Error-Message') ->
    diameter_gen_base_rfc6733:avp_header('Error-Message');
avp_header('Error-Reporting-Host') ->
    diameter_gen_base_rfc6733:avp_header('Error-Reporting-Host');
avp_header('Event-Timestamp') ->
    diameter_gen_base_rfc6733:avp_header('Event-Timestamp');
avp_header('Experimental-Result') ->
    diameter_gen_base_rfc6733:avp_header('Experimental-Result');
avp_header('Experimental-Result-Code') ->
    diameter_gen_base_rfc6733:avp_header('Experimental-Result-Code');
avp_header('Failed-AVP') ->
    diameter_gen_base_rfc6733:avp_header('Failed-AVP');
avp_header('Firmware-Revision') ->
    diameter_gen_base_rfc6733:avp_header('Firmware-Revision');
avp_header('Host-IP-Address') ->
    diameter_gen_base_rfc6733:avp_header('Host-IP-Address');
avp_header('Inband-Security-Id') ->
    diameter_gen_base_rfc6733:avp_header('Inband-Security-Id');
avp_header('Multi-Round-Time-Out') ->
    diameter_gen_base_rfc6733:avp_header('Multi-Round-Time-Out');
avp_header('Origin-Host') ->
    diameter_gen_base_rfc6733:avp_header('Origin-Host');
avp_header('Origin-Realm') ->
    diameter_gen_base_rfc6733:avp_header('Origin-Realm');
avp_header('Origin-State-Id') ->
    diameter_gen_base_rfc6733:avp_header('Origin-State-Id');
avp_header('Product-Name') ->
    diameter_gen_base_rfc6733:avp_header('Product-Name');
avp_header('Proxy-Host') ->
    diameter_gen_base_rfc6733:avp_header('Proxy-Host');
avp_header('Proxy-Info') ->
    diameter_gen_base_rfc6733:avp_header('Proxy-Info');
avp_header('Proxy-State') ->
    diameter_gen_base_rfc6733:avp_header('Proxy-State');
avp_header('Re-Auth-Request-Type') ->
    diameter_gen_base_rfc6733:avp_header('Re-Auth-Request-Type');
avp_header('Redirect-Host') ->
    diameter_gen_base_rfc6733:avp_header('Redirect-Host');
avp_header('Redirect-Host-Usage') ->
    diameter_gen_base_rfc6733:avp_header('Redirect-Host-Usage');
avp_header('Redirect-Max-Cache-Time') ->
    diameter_gen_base_rfc6733:avp_header('Redirect-Max-Cache-Time');
avp_header('Result-Code') ->
    diameter_gen_base_rfc6733:avp_header('Result-Code');
avp_header('Route-Record') ->
    diameter_gen_base_rfc6733:avp_header('Route-Record');
avp_header('Session-Binding') ->
    diameter_gen_base_rfc6733:avp_header('Session-Binding');
avp_header('Session-Id') ->
    diameter_gen_base_rfc6733:avp_header('Session-Id');
avp_header('Session-Server-Failover') ->
    diameter_gen_base_rfc6733:avp_header('Session-Server-Failover');
avp_header('Session-Timeout') ->
    diameter_gen_base_rfc6733:avp_header('Session-Timeout');
avp_header('Supported-Vendor-Id') ->
    diameter_gen_base_rfc6733:avp_header('Supported-Vendor-Id');
avp_header('Termination-Cause') ->
    diameter_gen_base_rfc6733:avp_header('Termination-Cause');
avp_header('User-Name') ->
    diameter_gen_base_rfc6733:avp_header('User-Name');
avp_header('Vendor-Id') ->
    diameter_gen_base_rfc6733:avp_header('Vendor-Id');
avp_header('Vendor-Specific-Application-Id') ->
    diameter_gen_base_rfc6733:avp_header('Vendor-Specific-Application-Id');
avp_header(_) -> erlang:error(badarg).

avp(T, Data, 'ARAP-Challenge-Response', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'ARAP-Features', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'ARAP-Password', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'ARAP-Security', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'ARAP-Security-Data', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'ARAP-Zone-Access', _) ->
    enumerated_avp(T, 'ARAP-Zone-Access', Data);
avp(T, Data, 'Accounting-Auth-Method', _) ->
    enumerated_avp(T, 'Accounting-Auth-Method', Data);
avp(T, Data, 'Accounting-Input-Octets', Opts) ->
    diameter_types:'Unsigned64'(T, Data, Opts);
avp(T, Data, 'Accounting-Input-Packets', Opts) ->
    diameter_types:'Unsigned64'(T, Data, Opts);
avp(T, Data, 'Accounting-Output-Octets', Opts) ->
    diameter_types:'Unsigned64'(T, Data, Opts);
avp(T, Data, 'Accounting-Output-Packets', Opts) ->
    diameter_types:'Unsigned64'(T, Data, Opts);
avp(T, Data, 'Acct-Authentic', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'Acct-Delay-Time', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'Acct-Link-Count', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'Acct-Session-Time', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'Acct-Tunnel-Connection', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Acct-Tunnel-Packets-Lost', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'CHAP-Algorithm', _) ->
    enumerated_avp(T, 'CHAP-Algorithm', Data);
avp(T, Data, 'CHAP-Auth', Opts) ->
    grouped_avp(T, 'CHAP-Auth', Data, Opts);
avp(T, Data, 'CHAP-Challenge', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'CHAP-Ident', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'CHAP-Response', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Callback-Id', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'Callback-Number', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'Called-Station-Id', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'Calling-Station-Id', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'Configuration-Token', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Connect-Info', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'Filter-Id', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'Framed-Appletalk-Link', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'Framed-Appletalk-Network', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'Framed-Appletalk-Zone', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Framed-Compression', _) ->
    enumerated_avp(T, 'Framed-Compression', Data);
avp(T, Data, 'Framed-IP-Address', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Framed-IP-Netmask', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Framed-IPX-Network', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'Framed-IPv6-Pool', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Framed-IPv6-Prefix', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Framed-IPv6-Route', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'Framed-Interface-Id', Opts) ->
    diameter_types:'Unsigned64'(T, Data, Opts);
avp(T, Data, 'Framed-MTU', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'Framed-Pool', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Framed-Protocol', _) ->
    enumerated_avp(T, 'Framed-Protocol', Data);
avp(T, Data, 'Framed-Route', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'Framed-Routing', _) ->
    enumerated_avp(T, 'Framed-Routing', Data);
avp(T, Data, 'Idle-Timeout', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'Login-IP-Host', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Login-IPv6-Host', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Login-LAT-Group', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Login-LAT-Node', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Login-LAT-Port', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Login-LAT-Service', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Login-Service', _) ->
    enumerated_avp(T, 'Login-Service', Data);
avp(T, Data, 'Login-TCP-Port', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'NAS-Filter-Rule', Opts) ->
    diameter_types:'IPFilterRule'(T, Data, Opts);
avp(T, Data, 'NAS-IP-Address', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'NAS-IPv6-Address', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'NAS-Identifier', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'NAS-Port', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'NAS-Port-Id', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'NAS-Port-Type', _) ->
    enumerated_avp(T, 'NAS-Port-Type', Data);
avp(T, Data, 'Origin-AAA-Protocol', _) ->
    enumerated_avp(T, 'Origin-AAA-Protocol', Data);
avp(T, Data, 'Originating-Line-Info', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Password-Retry', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'Port-Limit', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'Prompt', _) ->
    enumerated_avp(T, 'Prompt', Data);
avp(T, Data, 'QoS-Filter-Rule', Opts) ->
    diameter_types:'QoSFilterRule'(T, Data, Opts);
avp(T, Data, 'Reply-Message', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'Service-Type', _) ->
    enumerated_avp(T, 'Service-Type', Data);
avp(T, Data, 'State', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Tunnel-Assignment-Id', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Tunnel-Client-Auth-Id', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'Tunnel-Client-Endpoint', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'Tunnel-Medium-Type', _) ->
    enumerated_avp(T, 'Tunnel-Medium-Type', Data);
avp(T, Data, 'Tunnel-Password', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Tunnel-Preference', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'Tunnel-Private-Group-Id', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Tunnel-Server-Auth-Id', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'Tunnel-Server-Endpoint', Opts) ->
    diameter_types:'UTF8String'(T, Data, Opts);
avp(T, Data, 'Tunnel-Type', _) ->
    enumerated_avp(T, 'Tunnel-Type', Data);
avp(T, Data, 'Tunneling', Opts) ->
    grouped_avp(T, 'Tunneling', Data, Opts);
avp(T, Data, 'User-Password', Opts) ->
    diameter_types:'OctetString'(T, Data, Opts);
avp(T, Data, 'Accounting-Realtime-Required', Opts) ->
    avp(T, Data, 'Accounting-Realtime-Required', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Accounting-Record-Number', Opts) ->
    avp(T, Data, 'Accounting-Record-Number', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Accounting-Record-Type', Opts) ->
    avp(T, Data, 'Accounting-Record-Type', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Accounting-Sub-Session-Id', Opts) ->
    avp(T, Data, 'Accounting-Sub-Session-Id', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Acct-Application-Id', Opts) ->
    avp(T, Data, 'Acct-Application-Id', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Acct-Interim-Interval', Opts) ->
    avp(T, Data, 'Acct-Interim-Interval', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Acct-Multi-Session-Id', Opts) ->
    avp(T, Data, 'Acct-Multi-Session-Id', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Acct-Session-Id', Opts) ->
    avp(T, Data, 'Acct-Session-Id', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Auth-Application-Id', Opts) ->
    avp(T, Data, 'Auth-Application-Id', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Auth-Grace-Period', Opts) ->
    avp(T, Data, 'Auth-Grace-Period', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Auth-Request-Type', Opts) ->
    avp(T, Data, 'Auth-Request-Type', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Auth-Session-State', Opts) ->
    avp(T, Data, 'Auth-Session-State', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Authorization-Lifetime', Opts) ->
    avp(T, Data, 'Authorization-Lifetime', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Class', Opts) ->
    avp(T, Data, 'Class', Opts, diameter_gen_base_rfc6733);
avp(T, Data, 'Destination-Host', Opts) ->
    avp(T, Data, 'Destination-Host', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Destination-Realm', Opts) ->
    avp(T, Data, 'Destination-Realm', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Disconnect-Cause', Opts) ->
    avp(T, Data, 'Disconnect-Cause', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Error-Message', Opts) ->
    avp(T, Data, 'Error-Message', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Error-Reporting-Host', Opts) ->
    avp(T, Data, 'Error-Reporting-Host', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Event-Timestamp', Opts) ->
    avp(T, Data, 'Event-Timestamp', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Experimental-Result', Opts) ->
    grouped_avp(T, 'Experimental-Result', Data, Opts);
avp(T, Data, 'Experimental-Result-Code', Opts) ->
    avp(T, Data, 'Experimental-Result-Code', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Failed-AVP', Opts) ->
    grouped_avp(T, 'Failed-AVP', Data, Opts);
avp(T, Data, 'Firmware-Revision', Opts) ->
    avp(T, Data, 'Firmware-Revision', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Host-IP-Address', Opts) ->
    avp(T, Data, 'Host-IP-Address', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Inband-Security-Id', Opts) ->
    avp(T, Data, 'Inband-Security-Id', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Multi-Round-Time-Out', Opts) ->
    avp(T, Data, 'Multi-Round-Time-Out', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Origin-Host', Opts) ->
    avp(T, Data, 'Origin-Host', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Origin-Realm', Opts) ->
    avp(T, Data, 'Origin-Realm', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Origin-State-Id', Opts) ->
    avp(T, Data, 'Origin-State-Id', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Product-Name', Opts) ->
    avp(T, Data, 'Product-Name', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Proxy-Host', Opts) ->
    avp(T, Data, 'Proxy-Host', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Proxy-Info', Opts) ->
    grouped_avp(T, 'Proxy-Info', Data, Opts);
avp(T, Data, 'Proxy-State', Opts) ->
    avp(T, Data, 'Proxy-State', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Re-Auth-Request-Type', Opts) ->
    avp(T, Data, 'Re-Auth-Request-Type', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Redirect-Host', Opts) ->
    avp(T, Data, 'Redirect-Host', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Redirect-Host-Usage', Opts) ->
    avp(T, Data, 'Redirect-Host-Usage', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Redirect-Max-Cache-Time', Opts) ->
    avp(T, Data, 'Redirect-Max-Cache-Time', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Result-Code', Opts) ->
    avp(T, Data, 'Result-Code', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Route-Record', Opts) ->
    avp(T, Data, 'Route-Record', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Session-Binding', Opts) ->
    avp(T, Data, 'Session-Binding', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Session-Id', Opts) ->
    avp(T, Data, 'Session-Id', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Session-Server-Failover', Opts) ->
    avp(T, Data, 'Session-Server-Failover', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Session-Timeout', Opts) ->
    avp(T, Data, 'Session-Timeout', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Supported-Vendor-Id', Opts) ->
    avp(T, Data, 'Supported-Vendor-Id', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Termination-Cause', Opts) ->
    avp(T, Data, 'Termination-Cause', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'User-Name', Opts) ->
    avp(T, Data, 'User-Name', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Vendor-Id', Opts) ->
    avp(T, Data, 'Vendor-Id', Opts,
	diameter_gen_base_rfc6733);
avp(T, Data, 'Vendor-Specific-Application-Id', Opts) ->
    grouped_avp(T, 'Vendor-Specific-Application-Id', Data,
		Opts);
avp(_, _, _, _) -> erlang:error(badarg).

enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 0>>) ->
    0;
enumerated_avp(encode, 'NAS-Port-Type', 0) ->
    <<0, 0, 0, 0>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 1>>) ->
    1;
enumerated_avp(encode, 'NAS-Port-Type', 1) ->
    <<0, 0, 0, 1>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 2>>) ->
    2;
enumerated_avp(encode, 'NAS-Port-Type', 2) ->
    <<0, 0, 0, 2>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 3>>) ->
    3;
enumerated_avp(encode, 'NAS-Port-Type', 3) ->
    <<0, 0, 0, 3>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 4>>) ->
    4;
enumerated_avp(encode, 'NAS-Port-Type', 4) ->
    <<0, 0, 0, 4>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 5>>) ->
    5;
enumerated_avp(encode, 'NAS-Port-Type', 5) ->
    <<0, 0, 0, 5>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 6>>) ->
    6;
enumerated_avp(encode, 'NAS-Port-Type', 6) ->
    <<0, 0, 0, 6>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 7>>) ->
    7;
enumerated_avp(encode, 'NAS-Port-Type', 7) ->
    <<0, 0, 0, 7>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 8>>) ->
    8;
enumerated_avp(encode, 'NAS-Port-Type', 8) ->
    <<0, 0, 0, 8>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 9>>) ->
    9;
enumerated_avp(encode, 'NAS-Port-Type', 9) ->
    <<0, 0, 0, 9>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 10>>) ->
    10;
enumerated_avp(encode, 'NAS-Port-Type', 10) ->
    <<0, 0, 0, 10>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 11>>) ->
    11;
enumerated_avp(encode, 'NAS-Port-Type', 11) ->
    <<0, 0, 0, 11>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 12>>) ->
    12;
enumerated_avp(encode, 'NAS-Port-Type', 12) ->
    <<0, 0, 0, 12>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 13>>) ->
    13;
enumerated_avp(encode, 'NAS-Port-Type', 13) ->
    <<0, 0, 0, 13>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 14>>) ->
    14;
enumerated_avp(encode, 'NAS-Port-Type', 14) ->
    <<0, 0, 0, 14>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 15>>) ->
    15;
enumerated_avp(encode, 'NAS-Port-Type', 15) ->
    <<0, 0, 0, 15>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 16>>) ->
    16;
enumerated_avp(encode, 'NAS-Port-Type', 16) ->
    <<0, 0, 0, 16>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 17>>) ->
    17;
enumerated_avp(encode, 'NAS-Port-Type', 17) ->
    <<0, 0, 0, 17>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 18>>) ->
    18;
enumerated_avp(encode, 'NAS-Port-Type', 18) ->
    <<0, 0, 0, 18>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 19>>) ->
    19;
enumerated_avp(encode, 'NAS-Port-Type', 19) ->
    <<0, 0, 0, 19>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 20>>) ->
    20;
enumerated_avp(encode, 'NAS-Port-Type', 20) ->
    <<0, 0, 0, 20>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 21>>) ->
    21;
enumerated_avp(encode, 'NAS-Port-Type', 21) ->
    <<0, 0, 0, 21>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 22>>) ->
    22;
enumerated_avp(encode, 'NAS-Port-Type', 22) ->
    <<0, 0, 0, 22>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 23>>) ->
    23;
enumerated_avp(encode, 'NAS-Port-Type', 23) ->
    <<0, 0, 0, 23>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 24>>) ->
    24;
enumerated_avp(encode, 'NAS-Port-Type', 24) ->
    <<0, 0, 0, 24>>;
enumerated_avp(decode, 'NAS-Port-Type',
	       <<0, 0, 0, 25>>) ->
    25;
enumerated_avp(encode, 'NAS-Port-Type', 25) ->
    <<0, 0, 0, 25>>;
enumerated_avp(decode, 'Prompt', <<0, 0, 0, 0>>) -> 0;
enumerated_avp(encode, 'Prompt', 0) -> <<0, 0, 0, 0>>;
enumerated_avp(decode, 'Prompt', <<0, 0, 0, 1>>) -> 1;
enumerated_avp(encode, 'Prompt', 1) -> <<0, 0, 0, 1>>;
enumerated_avp(decode, 'CHAP-Algorithm',
	       <<0, 0, 0, 5>>) ->
    5;
enumerated_avp(encode, 'CHAP-Algorithm', 5) ->
    <<0, 0, 0, 5>>;
enumerated_avp(decode, 'Service-Type',
	       <<0, 0, 0, 1>>) ->
    1;
enumerated_avp(encode, 'Service-Type', 1) ->
    <<0, 0, 0, 1>>;
enumerated_avp(decode, 'Service-Type',
	       <<0, 0, 0, 2>>) ->
    2;
enumerated_avp(encode, 'Service-Type', 2) ->
    <<0, 0, 0, 2>>;
enumerated_avp(decode, 'Service-Type',
	       <<0, 0, 0, 3>>) ->
    3;
enumerated_avp(encode, 'Service-Type', 3) ->
    <<0, 0, 0, 3>>;
enumerated_avp(decode, 'Service-Type',
	       <<0, 0, 0, 4>>) ->
    4;
enumerated_avp(encode, 'Service-Type', 4) ->
    <<0, 0, 0, 4>>;
enumerated_avp(decode, 'Service-Type',
	       <<0, 0, 0, 5>>) ->
    5;
enumerated_avp(encode, 'Service-Type', 5) ->
    <<0, 0, 0, 5>>;
enumerated_avp(decode, 'Service-Type',
	       <<0, 0, 0, 6>>) ->
    6;
enumerated_avp(encode, 'Service-Type', 6) ->
    <<0, 0, 0, 6>>;
enumerated_avp(decode, 'Service-Type',
	       <<0, 0, 0, 7>>) ->
    7;
enumerated_avp(encode, 'Service-Type', 7) ->
    <<0, 0, 0, 7>>;
enumerated_avp(decode, 'Service-Type',
	       <<0, 0, 0, 8>>) ->
    8;
enumerated_avp(encode, 'Service-Type', 8) ->
    <<0, 0, 0, 8>>;
enumerated_avp(decode, 'Service-Type',
	       <<0, 0, 0, 9>>) ->
    9;
enumerated_avp(encode, 'Service-Type', 9) ->
    <<0, 0, 0, 9>>;
enumerated_avp(decode, 'Service-Type',
	       <<0, 0, 0, 10>>) ->
    10;
enumerated_avp(encode, 'Service-Type', 10) ->
    <<0, 0, 0, 10>>;
enumerated_avp(decode, 'Service-Type',
	       <<0, 0, 0, 11>>) ->
    11;
enumerated_avp(encode, 'Service-Type', 11) ->
    <<0, 0, 0, 11>>;
enumerated_avp(decode, 'Framed-Protocol',
	       <<0, 0, 0, 1>>) ->
    1;
enumerated_avp(encode, 'Framed-Protocol', 1) ->
    <<0, 0, 0, 1>>;
enumerated_avp(decode, 'Framed-Protocol',
	       <<0, 0, 0, 2>>) ->
    2;
enumerated_avp(encode, 'Framed-Protocol', 2) ->
    <<0, 0, 0, 2>>;
enumerated_avp(decode, 'Framed-Protocol',
	       <<0, 0, 0, 3>>) ->
    3;
enumerated_avp(encode, 'Framed-Protocol', 3) ->
    <<0, 0, 0, 3>>;
enumerated_avp(decode, 'Framed-Protocol',
	       <<0, 0, 0, 4>>) ->
    4;
enumerated_avp(encode, 'Framed-Protocol', 4) ->
    <<0, 0, 0, 4>>;
enumerated_avp(decode, 'Framed-Protocol',
	       <<0, 0, 0, 5>>) ->
    5;
enumerated_avp(encode, 'Framed-Protocol', 5) ->
    <<0, 0, 0, 5>>;
enumerated_avp(decode, 'Framed-Protocol',
	       <<0, 0, 0, 6>>) ->
    6;
enumerated_avp(encode, 'Framed-Protocol', 6) ->
    <<0, 0, 0, 6>>;
enumerated_avp(decode, 'Framed-Protocol',
	       <<0, 0, 0, 7>>) ->
    7;
enumerated_avp(encode, 'Framed-Protocol', 7) ->
    <<0, 0, 0, 7>>;
enumerated_avp(decode, 'Framed-Routing',
	       <<0, 0, 0, 0>>) ->
    0;
enumerated_avp(encode, 'Framed-Routing', 0) ->
    <<0, 0, 0, 0>>;
enumerated_avp(decode, 'Framed-Routing',
	       <<0, 0, 0, 1>>) ->
    1;
enumerated_avp(encode, 'Framed-Routing', 1) ->
    <<0, 0, 0, 1>>;
enumerated_avp(decode, 'Framed-Routing',
	       <<0, 0, 0, 2>>) ->
    2;
enumerated_avp(encode, 'Framed-Routing', 2) ->
    <<0, 0, 0, 2>>;
enumerated_avp(decode, 'Framed-Routing',
	       <<0, 0, 0, 3>>) ->
    3;
enumerated_avp(encode, 'Framed-Routing', 3) ->
    <<0, 0, 0, 3>>;
enumerated_avp(decode, 'Framed-Compression',
	       <<0, 0, 0, 0>>) ->
    0;
enumerated_avp(encode, 'Framed-Compression', 0) ->
    <<0, 0, 0, 0>>;
enumerated_avp(decode, 'Framed-Compression',
	       <<0, 0, 0, 1>>) ->
    1;
enumerated_avp(encode, 'Framed-Compression', 1) ->
    <<0, 0, 0, 1>>;
enumerated_avp(decode, 'Framed-Compression',
	       <<0, 0, 0, 2>>) ->
    2;
enumerated_avp(encode, 'Framed-Compression', 2) ->
    <<0, 0, 0, 2>>;
enumerated_avp(decode, 'Framed-Compression',
	       <<0, 0, 0, 3>>) ->
    3;
enumerated_avp(encode, 'Framed-Compression', 3) ->
    <<0, 0, 0, 3>>;
enumerated_avp(decode, 'ARAP-Zone-Access',
	       <<0, 0, 0, 1>>) ->
    1;
enumerated_avp(encode, 'ARAP-Zone-Access', 1) ->
    <<0, 0, 0, 1>>;
enumerated_avp(decode, 'ARAP-Zone-Access',
	       <<0, 0, 0, 2>>) ->
    2;
enumerated_avp(encode, 'ARAP-Zone-Access', 2) ->
    <<0, 0, 0, 2>>;
enumerated_avp(decode, 'ARAP-Zone-Access',
	       <<0, 0, 0, 3>>) ->
    3;
enumerated_avp(encode, 'ARAP-Zone-Access', 3) ->
    <<0, 0, 0, 3>>;
enumerated_avp(decode, 'ARAP-Zone-Access',
	       <<0, 0, 0, 4>>) ->
    4;
enumerated_avp(encode, 'ARAP-Zone-Access', 4) ->
    <<0, 0, 0, 4>>;
enumerated_avp(decode, 'Login-Service',
	       <<0, 0, 0, 0>>) ->
    0;
enumerated_avp(encode, 'Login-Service', 0) ->
    <<0, 0, 0, 0>>;
enumerated_avp(decode, 'Login-Service',
	       <<0, 0, 0, 1>>) ->
    1;
enumerated_avp(encode, 'Login-Service', 1) ->
    <<0, 0, 0, 1>>;
enumerated_avp(decode, 'Login-Service',
	       <<0, 0, 0, 2>>) ->
    2;
enumerated_avp(encode, 'Login-Service', 2) ->
    <<0, 0, 0, 2>>;
enumerated_avp(decode, 'Login-Service',
	       <<0, 0, 0, 3>>) ->
    3;
enumerated_avp(encode, 'Login-Service', 3) ->
    <<0, 0, 0, 3>>;
enumerated_avp(decode, 'Login-Service',
	       <<0, 0, 0, 4>>) ->
    4;
enumerated_avp(encode, 'Login-Service', 4) ->
    <<0, 0, 0, 4>>;
enumerated_avp(decode, 'Login-Service',
	       <<0, 0, 0, 5>>) ->
    5;
enumerated_avp(encode, 'Login-Service', 5) ->
    <<0, 0, 0, 5>>;
enumerated_avp(decode, 'Login-Service',
	       <<0, 0, 0, 6>>) ->
    6;
enumerated_avp(encode, 'Login-Service', 6) ->
    <<0, 0, 0, 6>>;
enumerated_avp(decode, 'Login-Service',
	       <<0, 0, 0, 7>>) ->
    7;
enumerated_avp(encode, 'Login-Service', 7) ->
    <<0, 0, 0, 7>>;
enumerated_avp(decode, 'Login-Service',
	       <<0, 0, 0, 8>>) ->
    8;
enumerated_avp(encode, 'Login-Service', 8) ->
    <<0, 0, 0, 8>>;
enumerated_avp(decode, 'Tunnel-Type', <<0, 0, 0, 1>>) ->
    1;
enumerated_avp(encode, 'Tunnel-Type', 1) ->
    <<0, 0, 0, 1>>;
enumerated_avp(decode, 'Tunnel-Type', <<0, 0, 0, 2>>) ->
    2;
enumerated_avp(encode, 'Tunnel-Type', 2) ->
    <<0, 0, 0, 2>>;
enumerated_avp(decode, 'Tunnel-Type', <<0, 0, 0, 3>>) ->
    3;
enumerated_avp(encode, 'Tunnel-Type', 3) ->
    <<0, 0, 0, 3>>;
enumerated_avp(decode, 'Tunnel-Type', <<0, 0, 0, 4>>) ->
    4;
enumerated_avp(encode, 'Tunnel-Type', 4) ->
    <<0, 0, 0, 4>>;
enumerated_avp(decode, 'Tunnel-Type', <<0, 0, 0, 5>>) ->
    5;
enumerated_avp(encode, 'Tunnel-Type', 5) ->
    <<0, 0, 0, 5>>;
enumerated_avp(decode, 'Tunnel-Type', <<0, 0, 0, 6>>) ->
    6;
enumerated_avp(encode, 'Tunnel-Type', 6) ->
    <<0, 0, 0, 6>>;
enumerated_avp(decode, 'Tunnel-Type', <<0, 0, 0, 7>>) ->
    7;
enumerated_avp(encode, 'Tunnel-Type', 7) ->
    <<0, 0, 0, 7>>;
enumerated_avp(decode, 'Tunnel-Type', <<0, 0, 0, 8>>) ->
    8;
enumerated_avp(encode, 'Tunnel-Type', 8) ->
    <<0, 0, 0, 8>>;
enumerated_avp(decode, 'Tunnel-Type', <<0, 0, 0, 9>>) ->
    9;
enumerated_avp(encode, 'Tunnel-Type', 9) ->
    <<0, 0, 0, 9>>;
enumerated_avp(decode, 'Tunnel-Type',
	       <<0, 0, 0, 10>>) ->
    10;
enumerated_avp(encode, 'Tunnel-Type', 10) ->
    <<0, 0, 0, 10>>;
enumerated_avp(decode, 'Tunnel-Type',
	       <<0, 0, 0, 11>>) ->
    11;
enumerated_avp(encode, 'Tunnel-Type', 11) ->
    <<0, 0, 0, 11>>;
enumerated_avp(decode, 'Tunnel-Type',
	       <<0, 0, 0, 12>>) ->
    12;
enumerated_avp(encode, 'Tunnel-Type', 12) ->
    <<0, 0, 0, 12>>;
enumerated_avp(decode, 'Tunnel-Type',
	       <<0, 0, 0, 13>>) ->
    13;
enumerated_avp(encode, 'Tunnel-Type', 13) ->
    <<0, 0, 0, 13>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 1>>) ->
    1;
enumerated_avp(encode, 'Tunnel-Medium-Type', 1) ->
    <<0, 0, 0, 1>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 2>>) ->
    2;
enumerated_avp(encode, 'Tunnel-Medium-Type', 2) ->
    <<0, 0, 0, 2>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 3>>) ->
    3;
enumerated_avp(encode, 'Tunnel-Medium-Type', 3) ->
    <<0, 0, 0, 3>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 4>>) ->
    4;
enumerated_avp(encode, 'Tunnel-Medium-Type', 4) ->
    <<0, 0, 0, 4>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 5>>) ->
    5;
enumerated_avp(encode, 'Tunnel-Medium-Type', 5) ->
    <<0, 0, 0, 5>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 6>>) ->
    6;
enumerated_avp(encode, 'Tunnel-Medium-Type', 6) ->
    <<0, 0, 0, 6>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 7>>) ->
    7;
enumerated_avp(encode, 'Tunnel-Medium-Type', 7) ->
    <<0, 0, 0, 7>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 8>>) ->
    8;
enumerated_avp(encode, 'Tunnel-Medium-Type', 8) ->
    <<0, 0, 0, 8>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 9>>) ->
    9;
enumerated_avp(encode, 'Tunnel-Medium-Type', 9) ->
    <<0, 0, 0, 9>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 10>>) ->
    10;
enumerated_avp(encode, 'Tunnel-Medium-Type', 10) ->
    <<0, 0, 0, 10>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 11>>) ->
    11;
enumerated_avp(encode, 'Tunnel-Medium-Type', 11) ->
    <<0, 0, 0, 11>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 12>>) ->
    12;
enumerated_avp(encode, 'Tunnel-Medium-Type', 12) ->
    <<0, 0, 0, 12>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 13>>) ->
    13;
enumerated_avp(encode, 'Tunnel-Medium-Type', 13) ->
    <<0, 0, 0, 13>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 14>>) ->
    14;
enumerated_avp(encode, 'Tunnel-Medium-Type', 14) ->
    <<0, 0, 0, 14>>;
enumerated_avp(decode, 'Tunnel-Medium-Type',
	       <<0, 0, 0, 15>>) ->
    15;
enumerated_avp(encode, 'Tunnel-Medium-Type', 15) ->
    <<0, 0, 0, 15>>;
enumerated_avp(decode, 'Accounting-Auth-Method',
	       <<0, 0, 0, 1>>) ->
    1;
enumerated_avp(encode, 'Accounting-Auth-Method', 1) ->
    <<0, 0, 0, 1>>;
enumerated_avp(decode, 'Accounting-Auth-Method',
	       <<0, 0, 0, 2>>) ->
    2;
enumerated_avp(encode, 'Accounting-Auth-Method', 2) ->
    <<0, 0, 0, 2>>;
enumerated_avp(decode, 'Accounting-Auth-Method',
	       <<0, 0, 0, 3>>) ->
    3;
enumerated_avp(encode, 'Accounting-Auth-Method', 3) ->
    <<0, 0, 0, 3>>;
enumerated_avp(decode, 'Accounting-Auth-Method',
	       <<0, 0, 0, 4>>) ->
    4;
enumerated_avp(encode, 'Accounting-Auth-Method', 4) ->
    <<0, 0, 0, 4>>;
enumerated_avp(decode, 'Accounting-Auth-Method',
	       <<0, 0, 0, 5>>) ->
    5;
enumerated_avp(encode, 'Accounting-Auth-Method', 5) ->
    <<0, 0, 0, 5>>;
enumerated_avp(decode, 'Origin-AAA-Protocol',
	       <<0, 0, 0, 1>>) ->
    1;
enumerated_avp(encode, 'Origin-AAA-Protocol', 1) ->
    <<0, 0, 0, 1>>;
enumerated_avp(_, _, _) -> erlang:error(badarg).

empty_value('CHAP-Auth', Opts) ->
    empty_group('CHAP-Auth', Opts);
empty_value('Tunneling', Opts) ->
    empty_group('Tunneling', Opts);
empty_value('Proxy-Info', Opts) ->
    empty_group('Proxy-Info', Opts);
empty_value('Failed-AVP', Opts) ->
    empty_group('Failed-AVP', Opts);
empty_value('Experimental-Result', Opts) ->
    empty_group('Experimental-Result', Opts);
empty_value('Vendor-Specific-Application-Id', Opts) ->
    empty_group('Vendor-Specific-Application-Id', Opts);
empty_value('NAS-Port-Type', _) -> <<0, 0, 0, 0>>;
empty_value('Prompt', _) -> <<0, 0, 0, 0>>;
empty_value('CHAP-Algorithm', _) -> <<0, 0, 0, 0>>;
empty_value('Service-Type', _) -> <<0, 0, 0, 0>>;
empty_value('Framed-Protocol', _) -> <<0, 0, 0, 0>>;
empty_value('Framed-Routing', _) -> <<0, 0, 0, 0>>;
empty_value('Framed-Compression', _) -> <<0, 0, 0, 0>>;
empty_value('ARAP-Zone-Access', _) -> <<0, 0, 0, 0>>;
empty_value('Login-Service', _) -> <<0, 0, 0, 0>>;
empty_value('Tunnel-Type', _) -> <<0, 0, 0, 0>>;
empty_value('Tunnel-Medium-Type', _) -> <<0, 0, 0, 0>>;
empty_value('Accounting-Auth-Method', _) ->
    <<0, 0, 0, 0>>;
empty_value('Origin-AAA-Protocol', _) -> <<0, 0, 0, 0>>;
empty_value('Disconnect-Cause', _) -> <<0, 0, 0, 0>>;
empty_value('Redirect-Host-Usage', _) -> <<0, 0, 0, 0>>;
empty_value('Auth-Request-Type', _) -> <<0, 0, 0, 0>>;
empty_value('Auth-Session-State', _) -> <<0, 0, 0, 0>>;
empty_value('Re-Auth-Request-Type', _) ->
    <<0, 0, 0, 0>>;
empty_value('Termination-Cause', _) -> <<0, 0, 0, 0>>;
empty_value('Session-Server-Failover', _) ->
    <<0, 0, 0, 0>>;
empty_value('Accounting-Record-Type', _) ->
    <<0, 0, 0, 0>>;
empty_value('Accounting-Realtime-Required', _) ->
    <<0, 0, 0, 0>>;
empty_value(Name, Opts) -> empty(Name, Opts).

dict() ->
    [1,
     {avp_types,
      [{"ARAP-Challenge-Response", 84, "OctetString", "MV"},
       {"ARAP-Features", 71, "OctetString", "MV"},
       {"ARAP-Password", 70, "OctetString", "MV"},
       {"ARAP-Security", 73, "Unsigned32", "MV"},
       {"ARAP-Security-Data", 74, "OctetString", "MV"},
       {"ARAP-Zone-Access", 72, "Enumerated", "MV"},
       {"Accounting-Auth-Method", 406, "Enumerated", "MV"},
       {"Accounting-Input-Octets", 363, "Unsigned64", "MV"},
       {"Accounting-Input-Packets", 365, "Unsigned64", "MV"},
       {"Accounting-Output-Octets", 364, "Unsigned64", "MV"},
       {"Accounting-Output-Packets", 366, "Unsigned64", "MV"},
       {"Acct-Authentic", 45, "Unsigned32", "MV"},
       {"Acct-Delay-Time", 41, "Unsigned32", "MV"},
       {"Acct-Link-Count", 51, "Unsigned32", "MV"},
       {"Acct-Session-Time", 46, "Unsigned32", "MV"},
       {"Acct-Tunnel-Connection", 68, "OctetString", "MV"},
       {"Acct-Tunnel-Packets-Lost", 86, "Unsigned32", "MV"},
       {"CHAP-Algorithm", 403, "Enumerated", "MV"},
       {"CHAP-Auth", 402, "Grouped", "MV"},
       {"CHAP-Challenge", 60, "OctetString", "MV"},
       {"CHAP-Ident", 404, "OctetString", "MV"},
       {"CHAP-Response", 405, "OctetString", "MV"},
       {"Callback-Id", 20, "UTF8String", "MV"},
       {"Callback-Number", 19, "UTF8String", "MV"},
       {"Called-Station-Id", 30, "UTF8String", "MV"},
       {"Calling-Station-Id", 31, "UTF8String", "MV"},
       {"Configuration-Token", 78, "OctetString", "MV"},
       {"Connect-Info", 77, "UTF8String", "MV"},
       {"Filter-Id", 11, "UTF8String", "MV"},
       {"Framed-Appletalk-Link", 37, "Unsigned32", "MV"},
       {"Framed-Appletalk-Network", 38, "Unsigned32", "MV"},
       {"Framed-Appletalk-Zone", 39, "OctetString", "M"},
       {"Framed-Compression", 13, "Enumerated", "MV"},
       {"Framed-IP-Address", 8, "OctetString", "MV"},
       {"Framed-IP-Netmask", 9, "OctetString", "MV"},
       {"Framed-IPX-Network", 23, "Unsigned32", "MV"},
       {"Framed-IPv6-Pool", 100, "OctetString", "MV"},
       {"Framed-IPv6-Prefix", 97, "OctetString", "MV"},
       {"Framed-IPv6-Route", 99, "UTF8String", "MV"},
       {"Framed-Interface-Id", 96, "Unsigned64", "MV"},
       {"Framed-MTU", 12, "Unsigned32", "MV"},
       {"Framed-Pool", 88, "OctetString", "MV"},
       {"Framed-Protocol", 7, "Enumerated", "MV"},
       {"Framed-Route", 22, "UTF8String", "MV"},
       {"Framed-Routing", 10, "Enumerated", "MV"},
       {"Idle-Timeout", 28, "Unsigned32", "MV"},
       {"Login-IP-Host", 14, "OctetString", "MV"},
       {"Login-IPv6-Host", 98, "OctetString", "MV"},
       {"Login-LAT-Group", 36, "OctetString", "MV"},
       {"Login-LAT-Node", 35, "OctetString", "MV"},
       {"Login-LAT-Port", 63, "OctetString", "MV"},
       {"Login-LAT-Service", 34, "OctetString", "MV"},
       {"Login-Service", 15, "Enumerated", "MV"},
       {"Login-TCP-Port", 16, "Unsigned32", "MV"},
       {"NAS-Filter-Rule", 400, "IPFilterRule", "MV"},
       {"NAS-IP-Address", 4, "OctetString", "M"},
       {"NAS-IPv6-Address", 95, "OctetString", "M"},
       {"NAS-Identifier", 32, "UTF8String", "M"},
       {"NAS-Port", 5, "Unsigned32", "MV"},
       {"NAS-Port-Id", 87, "UTF8String", "MV"},
       {"NAS-Port-Type", 61, "Enumerated", "MV"},
       {"Origin-AAA-Protocol", 408, "Enumerated", "M"},
       {"Originating-Line-Info", 94, "OctetString", "MV"},
       {"Password-Retry", 75, "Unsigned32", "MV"},
       {"Port-Limit", 62, "Unsigned32", "MV"},
       {"Prompt", 76, "Enumerated", "MV"},
       {"QoS-Filter-Rule", 407, "QoSFilterRule", []},
       {"Reply-Message", 18, "UTF8String", "MV"},
       {"Service-Type", 6, "Enumerated", "MV"},
       {"State", 24, "OctetString", "M"},
       {"Tunnel-Assignment-Id", 82, "OctetString", "MV"},
       {"Tunnel-Client-Auth-Id", 90, "UTF8String", "MV"},
       {"Tunnel-Client-Endpoint", 66, "UTF8String", "MV"},
       {"Tunnel-Medium-Type", 65, "Enumerated", "MV"},
       {"Tunnel-Password", 69, "OctetString", "MV"},
       {"Tunnel-Preference", 83, "Unsigned32", "MV"},
       {"Tunnel-Private-Group-Id", 81, "OctetString", "MV"},
       {"Tunnel-Server-Auth-Id", 91, "UTF8String", "MV"},
       {"Tunnel-Server-Endpoint", 67, "UTF8String", "MV"},
       {"Tunnel-Type", 64, "Enumerated", "MV"},
       {"Tunneling", 401, "Grouped", "MV"},
       {"User-Password", 2, "OctetString", "MV"}]},
     {avp_vendor_id, []}, {codecs, []},
     {command_codes,
      [{265, "AAR", "AAA"}, {271, "ACR", "ACA"},
       {258, "RAR", "RAA"}, {274, "ASR", "ASA"},
       {275, "STR", "STA"}]},
     {custom_types, []}, {define, []},
     {enum,
      [{"NAS-Port-Type",
	[{"ASYNC", 0}, {"SYNC", 1}, {"ISDN_SYNC", 2},
	 {"ISDN_ASYNC_V_120", 3}, {"ISDN_ASYNC_V_110", 4},
	 {"VIRTUAL", 5}, {"PIAFS", 6}, {"HDLC_CLEAR_CHANNEL", 7},
	 {"X_25", 8}, {"X_75", 9}, {"G_3_FAX", 10}, {"SDSL", 11},
	 {"ADSL_CAP", 12}, {"ADSL_DMT", 13}, {"IDSL", 14},
	 {"ETHERNET", 15}, {"XDSL", 16}, {"CABLE", 17},
	 {"WIRELESS_OTHER", 18}, {"WIRELESS_IEEE_802_11", 19},
	 {"TOKEN_RING", 20}, {"FDDI", 21},
	 {"WIRELESS_CDMA2000", 22}, {"WIRELESS_UMTS", 23},
	 {"WIRELESS_1X_EV", 24}, {"IAPP", 25}]},
       {"Prompt", [{"NO_ECHO", 0}, {"ECHO", 1}]},
       {"CHAP-Algorithm", [{"CHAP_WITH_MD5", 5}]},
       {"Service-Type",
	[{"LOGIN", 1}, {"FRAMED", 2}, {"CALLBACK_LOGIN", 3},
	 {"CALLBACK_FRAMED", 4}, {"OUTBOUND", 5},
	 {"ADMINISTRATIVE", 6}, {"NAS_PROMPT", 7},
	 {"AUTHENTICATE_ONLY", 8}, {"CALLBACK_NAS_PROMPT", 9},
	 {"CALL_CHECK", 10}, {"CALLBACK_ADMINISTRATIVE", 11}]},
       {"Framed-Protocol",
	[{"PPP", 1}, {"SLIP", 2}, {"ARAP", 3},
	 {"GANDALF__PROPRIETARY_PROTOCOL", 4},
	 {"XYLOGICS_IPX_SLIP", 5}, {"X_75_SYNCHRONOUS", 6},
	 {"GPRS_PDP_CONTEXT", 7}]},
       {"Framed-Routing",
	[{"NONE", 0}, {"SEND_ROUTING_PACKETS", 1},
	 {"LISTEN_FOR_ROUTING_PACKETS", 2},
	 {"SEND_AND_LISTEN", 3}]},
       {"Framed-Compression",
	[{"NONE", 0}, {"VJ_TCP_IP_HEADER_COMPRESSION", 1},
	 {"IPX_HEADER_COMPRESSION", 2},
	 {"STAC_LZS_COMPRESSION", 3}]},
       {"ARAP-Zone-Access",
	[{"ONLY_ACCESS_TO_DEFAULT_ZONE", 1},
	 {"USE_ZONE_FILTER_INCLUSIVELY", 2}, {"NOT_USED", 3},
	 {"USE_ZONE_FILTER_EXCLUSIVELY", 4}]},
       {"Login-Service",
	[{"TELNET", 0}, {"RLOGIN", 1}, {"TCP_CLEAR", 2},
	 {"PortMaster", 3}, {"LAT", 4}, {"X25_PAD", 5},
	 {"X25_T3POS", 6}, {"UNASSIGNED", 7},
	 {"TCP_CLEAR_QUIET", 8}]},
       {"Tunnel-Type",
	[{"PPTP", 1}, {"L2F", 2}, {"L2TP", 3}, {"ATMP", 4},
	 {"VTP", 5}, {"AH", 6}, {"IP_IP", 7}, {"MIN_IP_IP", 8},
	 {"ESP", 9}, {"GRE", 10}, {"DVS", 11},
	 {"IP_IN_IP_TUNNELING", 12}, {"VLAN", 13}]},
       {"Tunnel-Medium-Type",
	[{"IPV4", 1}, {"IPV6", 2}, {"NSAP", 3}, {"HDLC", 4},
	 {"BBN_1822", 5}, {"ALL_802_AND_ETHERNET", 6},
	 {"E_163", 7}, {"E_164", 8}, {"F_69", 9}, {"X_121", 10},
	 {"IPX", 11}, {"APPLETALK", 12}, {"DECNET_IV", 13},
	 {"BANYAN_VINES", 14},
	 {"E_164_NSAP_FORMAT_SUBADDRESS", 15}]},
       {"Accounting-Auth-Method",
	[{"PAP", 1}, {"CHAP", 2}, {"MS_CHAP_1", 3},
	 {"MS_CHAP_2", 4}, {"EAP", 5}]},
       {"Origin-AAA-Protocol", [{"RADIUS", 1}]}]},
     {grouped,
      [{"CHAP-Auth", 402, [],
	[{"CHAP-Algorithm"}, {"CHAP-Ident"}, ["CHAP-Response"],
	 {'*', ["AVP"]}]},
       {"Tunneling", 401, [],
	[{"Tunnel-Type"}, {"Tunnel-Medium-Type"},
	 {"Tunnel-Client-Endpoint"}, {"Tunnel-Server-Endpoint"},
	 ["Tunnel-Preference"], ["Tunnel-Client-Auth-Id"],
	 ["Tunnel-Server-Auth-Id"], ["Tunnel-Assignment-Id"],
	 ["Tunnel-Password"], ["Tunnel-Private-Group-Id"]]}]},
     {id, 1},
     {import_avps,
      [{diameter_gen_base_rfc6733,
	[{"Accounting-Realtime-Required", 483, "Enumerated",
	  "M"},
	 {"Accounting-Record-Number", 485, "Unsigned32", "M"},
	 {"Accounting-Record-Type", 480, "Enumerated", "M"},
	 {"Accounting-Sub-Session-Id", 287, "Unsigned64", "M"},
	 {"Acct-Application-Id", 259, "Unsigned32", "M"},
	 {"Acct-Interim-Interval", 85, "Unsigned32", "M"},
	 {"Acct-Multi-Session-Id", 50, "UTF8String", "M"},
	 {"Acct-Session-Id", 44, "OctetString", "M"},
	 {"Auth-Application-Id", 258, "Unsigned32", "M"},
	 {"Auth-Grace-Period", 276, "Unsigned32", "M"},
	 {"Auth-Request-Type", 274, "Enumerated", "M"},
	 {"Auth-Session-State", 277, "Enumerated", "M"},
	 {"Authorization-Lifetime", 291, "Unsigned32", "M"},
	 {"Class", 25, "OctetString", "M"},
	 {"Destination-Host", 293, "DiameterIdentity", "M"},
	 {"Destination-Realm", 283, "DiameterIdentity", "M"},
	 {"Disconnect-Cause", 273, "Enumerated", "M"},
	 {"Error-Message", 281, "UTF8String", []},
	 {"Error-Reporting-Host", 294, "DiameterIdentity", []},
	 {"Event-Timestamp", 55, "Time", "M"},
	 {"Experimental-Result", 297, "Grouped", "M"},
	 {"Experimental-Result-Code", 298, "Unsigned32", "M"},
	 {"Failed-AVP", 279, "Grouped", "M"},
	 {"Firmware-Revision", 267, "Unsigned32", []},
	 {"Host-IP-Address", 257, "Address", "M"},
	 {"Inband-Security-Id", 299, "Unsigned32", "M"},
	 {"Multi-Round-Time-Out", 272, "Unsigned32", "M"},
	 {"Origin-Host", 264, "DiameterIdentity", "M"},
	 {"Origin-Realm", 296, "DiameterIdentity", "M"},
	 {"Origin-State-Id", 278, "Unsigned32", "M"},
	 {"Product-Name", 269, "UTF8String", []},
	 {"Proxy-Host", 280, "DiameterIdentity", "M"},
	 {"Proxy-Info", 284, "Grouped", "M"},
	 {"Proxy-State", 33, "OctetString", "M"},
	 {"Re-Auth-Request-Type", 285, "Enumerated", "M"},
	 {"Redirect-Host", 292, "DiameterURI", "M"},
	 {"Redirect-Host-Usage", 261, "Enumerated", "M"},
	 {"Redirect-Max-Cache-Time", 262, "Unsigned32", "M"},
	 {"Result-Code", 268, "Unsigned32", "M"},
	 {"Route-Record", 282, "DiameterIdentity", "M"},
	 {"Session-Binding", 270, "Unsigned32", "M"},
	 {"Session-Id", 263, "UTF8String", "M"},
	 {"Session-Server-Failover", 271, "Enumerated", "M"},
	 {"Session-Timeout", 27, "Unsigned32", "M"},
	 {"Supported-Vendor-Id", 265, "Unsigned32", "M"},
	 {"Termination-Cause", 295, "Enumerated", "M"},
	 {"User-Name", 1, "UTF8String", "M"},
	 {"Vendor-Id", 266, "Unsigned32", "M"},
	 {"Vendor-Specific-Application-Id", 260, "Grouped",
	  "M"}]}]},
     {import_enums,
      [{diameter_gen_base_rfc6733,
	[{"Disconnect-Cause",
	  [{"REBOOTING", 0}, {"BUSY", 1},
	   {"DO_NOT_WANT_TO_TALK_TO_YOU", 2}]},
	 {"Redirect-Host-Usage",
	  [{"DONT_CACHE", 0}, {"ALL_SESSION", 1},
	   {"ALL_REALM", 2}, {"REALM_AND_APPLICATION", 3},
	   {"ALL_APPLICATION", 4}, {"ALL_HOST", 5},
	   {"ALL_USER", 6}]},
	 {"Auth-Request-Type",
	  [{"AUTHENTICATE_ONLY", 1}, {"AUTHORIZE_ONLY", 2},
	   {"AUTHORIZE_AUTHENTICATE", 3}]},
	 {"Auth-Session-State",
	  [{"STATE_MAINTAINED", 0}, {"NO_STATE_MAINTAINED", 1}]},
	 {"Re-Auth-Request-Type",
	  [{"AUTHORIZE_ONLY", 0}, {"AUTHORIZE_AUTHENTICATE", 1}]},
	 {"Termination-Cause",
	  [{"LOGOUT", 1}, {"SERVICE_NOT_PROVIDED", 2},
	   {"BAD_ANSWER", 3}, {"ADMINISTRATIVE", 4},
	   {"LINK_BROKEN", 5}, {"AUTH_EXPIRED", 6},
	   {"USER_MOVED", 7}, {"SESSION_TIMEOUT", 8}]},
	 {"Session-Server-Failover",
	  [{"REFUSE_SERVICE", 0}, {"TRY_AGAIN", 1},
	   {"ALLOW_SERVICE", 2}, {"TRY_AGAIN_ALLOW_SERVICE", 3}]},
	 {"Accounting-Record-Type",
	  [{"EVENT_RECORD", 1}, {"START_RECORD", 2},
	   {"INTERIM_RECORD", 3}, {"STOP_RECORD", 4}]},
	 {"Accounting-Realtime-Required",
	  [{"DELIVER_AND_GRANT", 1}, {"GRANT_AND_STORE", 2},
	   {"GRANT_AND_LOSE", 3}]}]}]},
     {import_groups,
      [{diameter_gen_base_rfc6733,
	[{"Proxy-Info", 284, [],
	  [{"Proxy-Host"}, {"Proxy-State"}, {'*', ["AVP"]}]},
	 {"Failed-AVP", 279, [], [{'*', {"AVP"}}]},
	 {"Experimental-Result", 297, [],
	  [{"Vendor-Id"}, {"Experimental-Result-Code"}]},
	 {"Vendor-Specific-Application-Id", 260, [],
	  [{"Vendor-Id"}, ["Auth-Application-Id"],
	   ["Acct-Application-Id"]]}]}]},
     {inherits, [{"diameter_gen_base_rfc6733", []}]},
     {messages,
      [{"AAR", 265, ['REQ', 'PXY'], [],
	[{{"Session-Id"}}, {"Auth-Application-Id"},
	 {"Origin-Host"}, {"Origin-Realm"},
	 {"Destination-Realm"}, {"Auth-Request-Type"},
	 ["Destination-Host"], ["NAS-Identifier"],
	 ["NAS-IP-Address"], ["NAS-IPv6-Address"], ["NAS-Port"],
	 ["NAS-Port-Id"], ["NAS-Port-Type"],
	 ["Origin-AAA-Protocol"], ["Origin-State-Id"],
	 ["Port-Limit"], ["User-Name"], ["User-Password"],
	 ["Service-Type"], ["State"], ["Authorization-Lifetime"],
	 ["Auth-Grace-Period"], ["Auth-Session-State"],
	 ["Callback-Number"], ["Called-Station-Id"],
	 ["Calling-Station-Id"], ["Originating-Line-Info"],
	 ["Connect-Info"], ["CHAP-Auth"], ["CHAP-Challenge"],
	 {'*', ["Framed-Compression"]}, ["Framed-Interface-Id"],
	 ["Framed-IP-Address"], {'*', ["Framed-IPv6-Prefix"]},
	 ["Framed-IP-Netmask"], ["Framed-MTU"],
	 ["Framed-Protocol"], ["ARAP-Password"],
	 ["ARAP-Security"], {'*', ["ARAP-Security-Data"]},
	 {'*', ["Login-IP-Host"]}, {'*', ["Login-IPv6-Host"]},
	 ["Login-LAT-Group"], ["Login-LAT-Node"],
	 ["Login-LAT-Port"], ["Login-LAT-Service"],
	 {'*', ["Tunneling"]}, {'*', ["Proxy-Info"]},
	 {'*', ["Route-Record"]}, {'*', ["AVP"]}]},
       {"AAA", 265, ['PXY'], [],
	[{{"Session-Id"}}, {"Auth-Application-Id"},
	 {"Auth-Request-Type"}, {"Result-Code"}, {"Origin-Host"},
	 {"Origin-Realm"}, ["User-Name"], ["Service-Type"],
	 {'*', ["Class"]}, {'*', ["Configuration-Token"]},
	 ["Acct-Interim-Interval"], ["Error-Message"],
	 ["Error-Reporting-Host"], {'*', ["Failed-AVP"]},
	 ["Idle-Timeout"], ["Authorization-Lifetime"],
	 ["Auth-Grace-Period"], ["Auth-Session-State"],
	 ["Re-Auth-Request-Type"], ["Multi-Round-Time-Out"],
	 ["Session-Timeout"], ["State"],
	 {'*', ["Reply-Message"]}, ["Origin-AAA-Protocol"],
	 ["Origin-State-Id"], {'*', ["Filter-Id"]},
	 ["Password-Retry"], ["Port-Limit"], ["Prompt"],
	 ["ARAP-Challenge-Response"], ["ARAP-Features"],
	 ["ARAP-Security"], {'*', ["ARAP-Security-Data"]},
	 ["ARAP-Zone-Access"], ["Callback-Id"],
	 ["Callback-Number"], ["Framed-Appletalk-Link"],
	 {'*', ["Framed-Appletalk-Network"]},
	 ["Framed-Appletalk-Zone"],
	 {'*', ["Framed-Compression"]}, ["Framed-Interface-Id"],
	 ["Framed-IP-Address"], {'*', ["Framed-IPv6-Prefix"]},
	 ["Framed-IPv6-Pool"], {'*', ["Framed-IPv6-Route"]},
	 ["Framed-IP-Netmask"], {'*', ["Framed-Route"]},
	 ["Framed-Pool"], ["Framed-IPX-Network"], ["Framed-MTU"],
	 ["Framed-Protocol"], ["Framed-Routing"],
	 {'*', ["Login-IP-Host"]}, {'*', ["Login-IPv6-Host"]},
	 ["Login-LAT-Group"], ["Login-LAT-Node"],
	 ["Login-LAT-Port"], ["Login-LAT-Service"],
	 ["Login-Service"], ["Login-TCP-Port"],
	 {'*', ["NAS-Filter-Rule"]}, {'*', ["QoS-Filter-Rule"]},
	 {'*', ["Tunneling"]}, {'*', ["Redirect-Host"]},
	 ["Redirect-Host-Usage"], ["Redirect-Max-Cache-Time"],
	 {'*', ["Proxy-Info"]}, {'*', ["AVP"]}]},
       {"RAR", 258, ['REQ', 'PXY'], [],
	[{{"Session-Id"}}, {"Origin-Host"}, {"Origin-Realm"},
	 {"Destination-Realm"}, {"Destination-Host"},
	 {"Auth-Application-Id"}, {"Re-Auth-Request-Type"},
	 ["User-Name"], ["Origin-AAA-Protocol"],
	 ["Origin-State-Id"], ["NAS-Identifier"],
	 ["NAS-IP-Address"], ["NAS-IPv6-Address"], ["NAS-Port"],
	 ["NAS-Port-Id"], ["NAS-Port-Type"], ["Service-Type"],
	 ["Framed-IP-Address"], ["Framed-IPv6-Prefix"],
	 ["Framed-Interface-Id"], ["Called-Station-Id"],
	 ["Calling-Station-Id"], ["Originating-Line-Info"],
	 ["Acct-Session-Id"], ["Acct-Multi-Session-Id"],
	 ["State"], {'*', ["Class"]}, ["Reply-Message"],
	 {'*', ["Proxy-Info"]}, {'*', ["Route-Record"]},
	 {'*', ["AVP"]}]},
       {"RAA", 258, ['PXY'], [],
	[{{"Session-Id"}}, {"Result-Code"}, {"Origin-Host"},
	 {"Origin-Realm"}, ["User-Name"],
	 ["Origin-AAA-Protocol"], ["Origin-State-Id"],
	 ["Error-Message"], ["Error-Reporting-Host"],
	 {'*', ["Failed-AVP"]}, {'*', ["Redirect-Host"]},
	 ["Redirect-Host-Usage"], ["Redirect-Max-Cache-Time"],
	 ["Service-Type"], {'*', ["Configuration-Token"]},
	 ["Idle-Timeout"], ["Authorization-Lifetime"],
	 ["Auth-Grace-Period"], ["Re-Auth-Request-Type"],
	 ["State"], {'*', ["Class"]}, {'*', ["Reply-Message"]},
	 ["Prompt"], {'*', ["Proxy-Info"]}, {'*', ["AVP"]}]},
       {"STR", 275, ['REQ', 'PXY'], [],
	[{{"Session-Id"}}, {"Origin-Host"}, {"Origin-Realm"},
	 {"Destination-Realm"}, {"Auth-Application-Id"},
	 {"Termination-Cause"}, ["User-Name"],
	 ["Destination-Host"], {'*', ["Class"]},
	 ["Origin-AAA-Protocol"], ["Origin-State-Id"],
	 {'*', ["Proxy-Info"]}, {'*', ["Route-Record"]},
	 {'*', ["AVP"]}]},
       {"STA", 275, ['PXY'], [],
	[{{"Session-Id"}}, {"Result-Code"}, {"Origin-Host"},
	 {"Origin-Realm"}, ["User-Name"], {'*', ["Class"]},
	 ["Error-Message"], ["Error-Reporting-Host"],
	 {'*', ["Failed-AVP"]}, ["Origin-AAA-Protocol"],
	 ["Origin-State-Id"], {'*', ["Redirect-Host"]},
	 ["Redirect-Host-Usage"], ["Redirect-Max-Cache-Time"],
	 {'*', ["Proxy-Info"]}, {'*', ["AVP"]}]},
       {"ASR", 274, ['REQ', 'PXY'], [],
	[{{"Session-Id"}}, {"Origin-Host"}, {"Origin-Realm"},
	 {"Destination-Realm"}, {"Destination-Host"},
	 {"Auth-Application-Id"}, ["User-Name"],
	 ["Origin-AAA-Protocol"], ["Origin-State-Id"],
	 ["NAS-Identifier"], ["NAS-IP-Address"],
	 ["NAS-IPv6-Address"], ["NAS-Port"], ["NAS-Port-Id"],
	 ["NAS-Port-Type"], ["Service-Type"],
	 ["Framed-IP-Address"], ["Framed-IPv6-Prefix"],
	 ["Framed-Interface-Id"], ["Called-Station-Id"],
	 ["Calling-Station-Id"], ["Originating-Line-Info"],
	 ["Acct-Session-Id"], ["Acct-Multi-Session-Id"],
	 ["State"], {'*', ["Class"]}, {'*', ["Reply-Message"]},
	 {'*', ["Proxy-Info"]}, {'*', ["Route-Record"]},
	 {'*', ["AVP"]}]},
       {"ASA", 274, ['PXY'], [],
	[{{"Session-Id"}}, {"Result-Code"}, {"Origin-Host"},
	 {"Origin-Realm"}, ["User-Name"],
	 ["Origin-AAA-Protocol"], ["Origin-State-Id"], ["State"],
	 ["Error-Message"], ["Error-Reporting-Host"],
	 {'*', ["Failed-AVP"]}, {'*', ["Redirect-Host"]},
	 ["Redirect-Host-Usage"], ["Redirect-Max-Cache-Time"],
	 {'*', ["Proxy-Info"]}, {'*', ["AVP"]}]},
       {"ACR", 271, ['REQ', 'PXY'], [],
	[{{"Session-Id"}}, {"Origin-Host"}, {"Origin-Realm"},
	 {"Destination-Realm"}, {"Accounting-Record-Type"},
	 {"Accounting-Record-Number"}, {"Acct-Application-Id"},
	 ["User-Name"], ["Accounting-Sub-Session-Id"],
	 ["Acct-Session-Id"], ["Acct-Multi-Session-Id"],
	 ["Origin-AAA-Protocol"], ["Origin-State-Id"],
	 ["Destination-Host"], ["Event-Timestamp"],
	 ["Acct-Delay-Time"], ["NAS-Identifier"],
	 ["NAS-IP-Address"], ["NAS-IPv6-Address"], ["NAS-Port"],
	 ["NAS-Port-Id"], ["NAS-Port-Type"], {'*', ["Class"]},
	 ["Service-Type"], ["Termination-Cause"],
	 ["Accounting-Input-Octets"],
	 ["Accounting-Input-Packets"],
	 ["Accounting-Output-Octets"],
	 ["Accounting-Output-Packets"], ["Acct-Authentic"],
	 ["Accounting-Auth-Method"], ["Acct-Link-Count"],
	 ["Acct-Session-Time"], ["Acct-Tunnel-Connection"],
	 ["Acct-Tunnel-Packets-Lost"], ["Callback-Id"],
	 ["Callback-Number"], ["Called-Station-Id"],
	 ["Calling-Station-Id"], {'*', ["Connect-Info"]},
	 ["Originating-Line-Info"], ["Authorization-Lifetime"],
	 ["Session-Timeout"], ["Idle-Timeout"], ["Port-Limit"],
	 ["Accounting-Realtime-Required"],
	 ["Acct-Interim-Interval"], {'*', ["Filter-Id"]},
	 {'*', ["NAS-Filter-Rule"]}, {'*', ["QoS-Filter-Rule"]},
	 ["Framed-Appletalk-Link"], ["Framed-Appletalk-Network"],
	 ["Framed-Appletalk-Zone"], ["Framed-Compression"],
	 ["Framed-Interface-Id"], ["Framed-IP-Address"],
	 ["Framed-IP-Netmask"], {'*', ["Framed-IPv6-Prefix"]},
	 ["Framed-IPv6-Pool"], {'*', ["Framed-IPv6-Route"]},
	 ["Framed-IPX-Network"], ["Framed-MTU"], ["Framed-Pool"],
	 ["Framed-Protocol"], {'*', ["Framed-Route"]},
	 ["Framed-Routing"], {'*', ["Login-IP-Host"]},
	 {'*', ["Login-IPv6-Host"]}, ["Login-LAT-Group"],
	 ["Login-LAT-Node"], ["Login-LAT-Port"],
	 ["Login-LAT-Service"], ["Login-Service"],
	 ["Login-TCP-Port"], {'*', ["Tunneling"]},
	 {'*', ["Proxy-Info"]}, {'*', ["Route-Record"]},
	 {'*', ["AVP"]}]},
       {"ACA", 271, ['PXY'], [],
	[{{"Session-Id"}}, {"Result-Code"}, {"Origin-Host"},
	 {"Origin-Realm"}, {"Accounting-Record-Type"},
	 {"Accounting-Record-Number"}, {"Acct-Application-Id"},
	 ["User-Name"], ["Accounting-Sub-Session-Id"],
	 ["Acct-Session-Id"], ["Acct-Multi-Session-Id"],
	 ["Event-Timestamp"], ["Error-Message"],
	 ["Error-Reporting-Host"], {'*', ["Failed-AVP"]},
	 ["Origin-AAA-Protocol"], ["Origin-State-Id"],
	 ["NAS-Identifier"], ["NAS-IP-Address"],
	 ["NAS-IPv6-Address"], ["NAS-Port"], ["NAS-Port-Id"],
	 ["NAS-Port-Type"], ["Service-Type"],
	 ["Termination-Cause"], ["Accounting-Realtime-Required"],
	 ["Acct-Interim-Interval"], {'*', ["Class"]},
	 {'*', ["Proxy-Info"]}, {'*', ["AVP"]}]}]},
     {name, "diameter_gen_nas_application_rfc7155"},
     {prefix, "diameter_nas_app"}, {vendor, {0, "IETF"}}].


