require "accounting/packages/user_packages"

up = UserPackages:create("msd",{})
return up:save()