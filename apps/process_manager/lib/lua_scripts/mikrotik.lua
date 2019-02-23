Framed_Protocols = {
    PPP = 1,
    SLIP  = 2
}
Framed_Comporession = {
    Van_Jacobsen_TCP_IP = 1
}

Mikrotik = {}
Mikrotik.__index = Mikrotik

function Mikrotik:create() 
	local rtn = {}
	setmetatable(rtn,Mikrotik)
	return rtn
end

function Mikrotik:limit_rx_rate(rate_limit)
   self.a_rx_rate = tostring(rate_limit)
   return self
end


function Mikrotik:limit_tx_rate(rate_limit)
    self.a_tx_rate = tostring(rate_limit)
    return self
end

function Mikrotik:framed_proto(framed_proto) 
    self.a_framed_proto = framed_proto
    return self
end

function Mikrotik:framed_proto_compression(v)
    self.a_framed_proto_compression = v
    return self
end

function Mikrotik:Value()
    local rtn = {}
    if (self.a_framed_proto) then
        rtn[7] = self.a_framed_proto
    end
    if (self.a_framed_proto_compression) then
        rtn[13] = self.a_framed_proto_compression
    end

    return rtn
end