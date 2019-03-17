function list_length(l)
    if type(l) == "table" then
        local count = 0
        for _, _ in pairs(l) do
            count = count + 1
        end
        return count
    end
    return -1
end

function list_contains(list, item)
    if type(list) == "table" then
        for _, v in pairs(list) do
            if v == item then
                return true
            end
        end
        return false
    else
        return false
    end
end
