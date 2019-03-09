function is_instanceof(object, klass)
    local m = getmetatable(object)
    local ref_meta_table = klass.get_class_metatable()
    while m do
        if m == ref_meta_table then
            return true
        end

        m = getmetatable(m)
    end
    return false
end
