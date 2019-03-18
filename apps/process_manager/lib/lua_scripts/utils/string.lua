function random_string(len)
    math.randomseed(os.time())

    return tostring(math.random(10000000000000))
end
