
function is_number( v )
    return type(v) == "number"
end

function is_integer(v) 
    i,f = math.modf( v )
    return f == 0 
end

function is_positive(v)
    return v > 0 
end

function is_negative(v)
    return v<0
end
