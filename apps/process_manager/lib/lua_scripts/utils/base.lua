--- Filter an iterator with a predicate.
-- @param p predicate
-- @param i iterator
-- @return result table containing elements e for which p (e)
function _G.filter(p, i, ...)
    local t = {}
    for e in i(...) do
        if p(e) then
            table.insert(t, e)
        end
    end
    return t
end
