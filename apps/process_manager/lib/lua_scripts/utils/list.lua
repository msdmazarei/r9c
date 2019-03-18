require("utils/base")
function list_length(l)
    if type(l) == "table" then
       return #l
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

--- An iterator over the elements of a list.
-- @param l list to iterate over
-- @return iterator function which returns successive elements of the list
-- @return the list <code>l</code> as above
-- @return <code>true</code>
function list_elems(l)
    local n = 0
    return function(l)
        n = n + 1
        if n <= #l then
            return l[n]
        end
    end, l, true
end


--- Filter a list according to a predicate.
-- @param p predicate (function of one argument returning a boolean)
-- @param l list of lists
-- @return result list containing elements <code>e</code> of
--   <code>l</code> for which <code>p (e)</code> is true
function list_filter(p, l)
    return _G.filter(p, list_elems, l)
end


--- Append an item to a list.
-- @param l table
-- @param x item
-- @return <code>{l[1], ..., l[#l], x}</code>
function list_append (l, x)
    local r = {unpack (l)}
    table.insert (r, x)
    return r
  end
  