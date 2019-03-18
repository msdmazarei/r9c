function is_instanceof(object, klass)
    print("is_instanceof called.")
    local m = getmetatable(object)
    local ref_meta_table = klass.get_class_metatable()
    while m do
        print("m.__msd_class_name", m.__msd_class_name, "ref_meta_table.__msd_class_name",ref_meta_table.__msd_class_name)
        if m == ref_meta_table or m.__msd_class_name==ref_meta_table.__msd_class_name then
            print("is_instanceof returnrd true")
            return true
        end

        m = getmetatable(m)
    end
    print("is_instanceof returnrd false")
    return false
end
