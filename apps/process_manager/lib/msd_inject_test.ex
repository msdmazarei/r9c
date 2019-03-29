defmodule ProcessManager.Injecttion.Msd do
  def f1_(args, state) do
    :io.fwrite("this is f1 function which called.~n")
    {[true], state}
  end

  def test_1() do
    f1 = fn args, state ->
      :io.fwrite("this is f1 function which called.~n")
      {[true], state}
    end

    f2 = fn args, state ->
      :io.fwrite("f2 is called with args:~p~n", [args])
      {[true], state}
    end

    s0 = :luerl.init()
    # add f1 function to state
    s1 = :luerl.set_table(["f1"], f1, s0)

    s1 = :luerl.set_table(["f2"], f2, s1)

    # s1 = :luerl.set_table([:msd], Map.to_list(%{"f2" => f2}), s1)

    {f1_path, _} = :luerl.do("return f1", s1)
    :io.fwrite("f1_path:~p~n", [f1_path])

    {r, s2} =
      :luerl.do(
        """
        f2(f1)
          function j(f)
          if(type(f)=="table") then
          f["function"]()
        end
        if type(f)=="function" then
          print("f is function let call it")
          f()
        else
           print("type f is",type(f) )
        end
        end



        """,
        s1
      )

    # {encoded, _} = :luerl.encode(f1_path, s2)
    encoded = f1_path
    :io.fwrite("f1_path;~p~n", [encoded])
    {f1_path, :luerl.call_function(["j"], [encoded], s2)}
  end
end
