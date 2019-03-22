require("accounting/packages/package_definition")
username = "msd"
pkg_name = "regular_adsl_internet_ratio"
act_pkg_vars = {
    speed = 1024 * 1024, -- 1Mbps
    traffic = 1024 * 1024 * 1024 * 3, --3G
    duration = 3600 * 30 -- 30 days
}

pkg_def = PackageDefinition.get_from_db(pkg_name)
print("here i am")

if pkg_def == nil then
    print("no package found")
    return false, "package not found"
else
    cel.kvdb.transaction(
        function()
            success, res =
                pkg_def:assign_to_user(
                "nsm",
                "regular_adsl_internet_ratio_t1",
                1,
                {
                    speed = 1024 * 1024, -- 1Mbps
                    traffic = 1024 * 1024 * 1024 * 3, --3G
                    duration = 3600 * 3 -- 30 days
                }
            )
            if success == false then
                print("problem to activate package for user")
                return false, "problem to activate"
            else
                print("successfully done")
                return true, "ok"
            end
        end,
        {},
        "infinity"
    )
end
