defmodule ApiserverWeb.Admin.Settings.Nodes.Controller do
  use ApiserverWeb.RCBaseController, :crud

  plug :request_body_validation

  require Logger
  require Utilities.Logging
  alias Utilities.Logging

  require DatabaseEngine.Interface.SystemConfig.NodeModel
  alias DatabaseEngine.Interface.SystemConfig.NodeModel

  require DatabaseEngine.Interface.SystemConfig.Node.Repo
  alias DatabaseEngine.Interface.SystemConfig.Node.Repo

  alias Utilities.Admin.Node, as: NodeAdmin

  @api_duration "api_duration"

  def list_active_nodes_on_target_node(conn, params) do
    st_time = Utilities.now()

    case params["nodename"] do
      nil ->
        Logging.error("no nodename passed. returns 404")
        conn |> send_response(404, nil)

      nodename ->
        nodename_atom = nodename |> String.to_atom()

        case :rpc.block_call(nodename_atom, Utilities, :all_active_nodes, [], 5_000) do
          {:badrpc, reason} ->
            reason = :io_lib.format("~p", [reason]) |> Utilities.erl_list_to_iex_string()

            rtn = %{
              "_meta_" => %{
                @api_duration => Utilities.now() - st_time
              },
              "result" => %{
                "error" => reason
              }
            }

            Logging.error("problem to retrive active_node_names on node:~p", [nodename])
            conn |> send_response(500, rtn)

          v ->
            rtn = %{
              "_meta_" => %{
                @api_duration => Utilities.now() - st_time
              },
              "result" => v
            }

            Logging.info("Done. active nodes retrived for node:~p", [nodename])
            conn |> send_response(200, rtn)
        end
    end
  end

  def list_active_nodes(conn, _params) do
    Logging.debug("called. ")
    active_nodes = Utilities.all_active_nodes()

    rtn = %{
      "result" => active_nodes
    }

    Logging.debug("returns: ~p", [rtn])
    Logging.info("Done.")

    conn
    |> json(rtn)
  end

  def ping(conn, params) do
    nodename = params["nodename"]
    nodename_atom = nodename |> String.to_atom()
    result = :net_adm.ping(nodename_atom)

    Logging.info("Called. node name:~p, result:~p", [nodename, result])

    case result do
      :pong ->
        conn |> send_resp(204, "")

      :pang ->
        conn |> send_resp(404, "")
    end
  end

  def connect_to_node(conn, params) do
    st_time = Utilities.now()
    Logging.debug("called. params:~p", [params])

    case params["nodename"] do
      nil ->
        Logging.info("no node name passed to function. returns 404")
        conn |> send_response(404, nil)

      nodename ->
        nodename_atom = nodename |> String.to_atom()

        result =
          case :net_kernel.connect_node(nodename_atom) do
            :ignored -> false
            v when is_boolean(v) -> v
          end

        du_time = Utilities.now() - st_time
        rtn = %{"result" => result, "_meta_" => %{@api_duration => du_time}}
        Logging.info("Called. nodename:~p result:~p", [nodename_atom, rtn])
        conn |> json(rtn)
    end
  end

  def disconnect_node(conn, params) do
    st_time = Utilities.now()

    case params["nodename"] do
      nil ->
        Logging.info("called with no node name. returns 404")
        conn |> send_response(404, %{"nodename" => "required"})

      nodename ->
        nodename_atom = nodename |> String.to_atom()

        result =
          case :net_kernel.disconnect(nodename_atom) do
            :ignored -> false
            v when is_boolean(v) -> v
          end

        Logging.info("called. nodename:~p result:~p", [nodename, result])

        conn
        |> send_response(200, %{
          "result" => result,
          "_meta_" => %{
            @api_duration => Utilities.now() - st_time
          }
        })
    end
  end

  def node_brief(conn, params) do
    Logging.debug("called.")
    nodename = params["nodename"]
    st_time = Utilities.now()

    nodename_atom = nodename |> String.to_atom()

    case :rpc.block_call(nodename_atom, NodeAdmin, :brief_node_info, [], 5_000) do
      {:badrpc, reason} ->
        Logging.error("problem to rpc call:~p", [reason])
        du_time = Utilities.now() - st_time
        reason = :io_lib.format("~p", [reason]) |> Utilities.erl_list_to_iex_string()

        rtn = %{
          "_meta_" => %{
            @api_duration => du_time
          },
          "result" => reason
        }

        conn |> send_response(500, rtn)

      value ->
        du_time = Utilities.now() - st_time
        Logging.info("nodename:~p called", [nodename])

        rtn = %{
          "result" => value,
          "_meta_" => %{
            @api_duration => du_time
          }
        }

        conn |> send_response(200, rtn)
    end
  end

  def set_cookie(conn, params) do
    st_time = Utilities.now()
    cookie = conn.body_params["cookie"] |> String.to_atom()

    case params["nodename"] do
      nil ->
        Logging.error("no nodename passed. returns 404")
        conn |> send_response(404, nil)

      nodename ->
        nodename_atom = nodename |> String.to_atom()

        case :rpc.block_call(nodename_atom, :erlang, :set_cookie, [nodename_atom, cookie], 5_000) do
          {:badrpc, reason} ->
            Logging.error("error to set cookie. reason: ~p", [reason])
            du_time = Utilities.now() - st_time

            reason = :io_lib.format("~p", [reason]) |> Utilities.erl_list_to_iex_string()

            rtn = %{
              "_meta_" => %{
                @api_duration => du_time
              },
              "result" => %{
                "error" => reason
              }
            }

            conn |> send_response(500, rtn)

          result ->
            du_time = Utilities.now() - st_time

            rtn = %{
              "_meta_" => %{
                @api_duration => du_time
              },
              "result" => result
            }

            Logging.info("successfully cookie:~p set to node:~p", [cookie, nodename])
            conn |> send_response(200, rtn)
        end
    end
  end

  def transfer_all_local_modules_to_target_node(conn, params) do
    st_time = Utilities.now()

    case params["nodename"] do
      nil ->
        du_time = Utilities.now() - st_time

        rtn = %{
          "_meta_" => %{
            @api_duration => du_time
          },
          "result" => %{
            "nodename" => "required"
          }
        }

        Logging.error("bad request. nodename is missing")
        conn |> send_response(400, rtn)

      nodename when is_binary(nodename) ->
        nodename_atom = nodename |> String.to_atom()
        modules_to_transfer = NodeAdmin.get_all_modules_bin()

        case modules_to_transfer do
          nil ->
            du_time = Utilities.now() - st_time

            rtn = %{
              "_meta_" => %{
                @api_duration => du_time
              },
              "result" => %{
                "error" => "problem to retrive modules to tansfer"
              }
            }

            Logging.error("problem to retrive module_to_transfer")
            conn |> send_response(500, rtn)

          _ ->
            case NodeAdmin.load_modules_to_target_node(nodename_atom, modules_to_transfer) do
              :ok ->
                du_time = Utilities.now() - st_time

                rtn = %{
                  "_meta_" => %{
                    @api_duration => du_time
                  },
                  "result" => %{
                    "modules_count" => modules_to_transfer |> length
                  }
                }

                Logging.info("successfully modules transfered to node:~p", [nodename])
                conn |> send_response(200, rtn)

              {:badrpc, reason} ->
                Logging.debug("problem to rpc call:~p", [reason])
                reason = :io_lib.format("~p", [reason]) |> Utilities.erl_list_to_iex_string()

                du_time = Utilities.now() - st_time

                rtn = %{
                  "_meta_" => %{
                    @api_duration => du_time
                  },
                  "result" => %{
                    "error" => reason
                  }
                }

                conn |> send_response(500, rtn)
            end
        end
    end
  end

  def badrpc_result(st_time, reason) do
    rtn = %{
      "_meta_" => %{
        @api_duration => Utilities.now() - st_time
      },
      "result" => %{
        "error" => :io_lib.format("~p", [reason]) |> Utilities.erl_list_to_iex_string()
      }
    }

    rtn
  end

  def permanently_set_cookie_target_node(conn, params) do
    nodename = params["nodename"]
    cookie_to_connect = params["cookie_to_connect"]
    cookie_to_save = params["cookie_to_save"]
    st_time = Utilities.now()

    case NodeAdmin.save_cookie_to_target_node(nodename, cookie_to_connect, cookie_to_save) do
      {:badrpc, reason} ->
        rtn = badrpc_result(st_time, reason)
        conn |> send_response(504, rtn)

      {:error, reason} ->
        rtn = badrpc_result(st_time, reason)
        conn |> send_response(500, rtn)

      :ok ->
        rtn = %{
          "_meta_" => %{
            @api_duration => Utilities.now() - st_time
          }
        }

        conn
        |> send_response(200, rtn)
    end
  end

  def get_all_loaded_modules(conn, params) do
    st_time = Utilities.now()

    case params["nodename"] do
      nil ->
        Logging.error("nodename require.")

        rtn = %{
          "_meta_" => %{
            @api_duration => Utilities.now() - st_time
          },
          "error" => %{
            "nodename" => "required"
          }
        }

        conn |> send_response(400, rtn)

      nodename ->
        result = NodeAdmin.get_all_loaded_modules(nodename)

        case result do
          {:badrpc, reason} ->
            rtn = badrpc_result(st_time, reason)
            conn |> send_response(conn, rtn)

          _ ->
            res = result |> Enum.map(fn {n, _} -> n end)

            rtn = %{
              "_meta_" => %{
                @api_duration => Utilities.now() - st_time
              },
              "result" => res
            }

            Logging.info("loaded module for node:~p are totally:~p", [nodename, res |> length])
            conn |> send_response(200, rtn)
        end
    end
  end

  def check_is_node_proper_to_run_r9c(conn, params) do
    st_time = Utilities.now()

    nodename =
      case params["nodename"] do
        v when is_binary(v) -> v |> String.to_atom()
        a when is_atom(a) -> a
      end

    case NodeAdmin.get_all_loaded_modules(nodename) do
      {:badrpc, reason} ->
        rtn = badrpc_result(st_time, reason)
        conn |> send_response(500, rtn)

      v ->
        app_modules = NodeAdmin.get_transferable_modules() |> Enum.map(fn {n, _} -> n end)
        remote_modules = v |> Enum.map(fn {n, _} -> n end)

        require_modules =
          app_modules
          |> Enum.filter(fn x ->
            !Enum.member?(remote_modules, x)
          end)

        # check require modules are exits
        code_which =
          require_modules
          |> Enum.map(fn x ->
            :rpc.block_call(nodename, :code, :which, [x], 1_000)
          end)

        badrpc_errors =
          code_which
          |> Enum.filter(fn x ->
            case x do
              {:badrpc, _} -> true
              _ -> false
            end
          end)

        case badrpc_errors do
          [a | _] ->
            rtn = badrpc_result(st_time, a |> elem(1))

            conn |> send_response(500, rtn)

          _ ->
            if Enum.find(code_which, fn x -> x == :non_existing end) != nil do
              require_modules =
                Enum.zip(code_which, require_modules)
                |> Enum.filter(fn {x, _} ->
                  case x do
                    :non_existing -> true
                    _ -> false
                  end
                end)
                |> Enum.map(fn {_, x} -> x end)

              rtn = %{
                "_meta_" => %{
                  @api_duration => Utilities.now() - st_time
                },
                "result" => %{
                  "require_modules" => require_modules
                }
              }

              conn |> send_response(410, rtn)
            else
              rtn = %{
                "_meta_" => %{
                  @api_duration => Utilities.now() - st_time
                },
                "result" => true
              }

              conn |> send_response(200, rtn)
            end
        end
    end
  end

  def map_to_node_struct(params) do
    node_instance = Utilities.to_struct(NodeModel, params)

    node_instance = %{
      node_instance
      | cookie:
          if is_binary(node_instance.cookie) do
            node_instance.cookie |> String.to_atom()
          else
            node_instance.cookie
          end,
        connected_nodes:
          (node_instance.connected_nodes || [])
          |> Enum.map(fn x ->
            if is_atom(x) do
              x
            else
              x |> String.to_atom()
            end
          end)
    }

    node_instance
  end

  def add_new_node_to_config(conn, params) do
    st_time = Utilities.now()
    Logging.debug("called. params:~p", [params])

    node_instance = map_to_node_struct(params)

    db_result =
      try do
        Repo.add_new_node(node_instance)
      catch
        {:aborted, v} ->
          {:aborted, v}
      end

    Logging.debug("db_result:~p", [db_result])

    {status, rtn} =
      case db_result do
        {:aborted, :already_node_exists} ->
          Logging.info("node was already registered.")

          {409,
           %{
             "node_name" => "uniq"
           }}

        {:aborted, :invalid_model} ->
          Logging.info("bad request data")

          {400,
           %{
             "request_body" => "bad model"
           }}

        {:atomic, model} ->
          Logging.info("successfully done")
          {200, model |> Map.from_struct()}

        {:aborted, e} ->
          Logging.error("unpredictaed excpetion happend. error:~p", [e])
          {500, %{"error" => "unhandled error"}}
      end

    result = %{
      "_meta_" => %{
        @api_duration => Utilities.now() - st_time
      },
      "result" => rtn
    }

    conn |> send_response(status, result)
  end

  def del_node_config(conn, params) do
    st_time = Utilities.now()
    Logging.debug("Called. params:~p", [params])
    nodename = params["nodename"]

    {status, rtn} =
      case Repo.del_node(nodename) do
        {:atomic, n} ->
          Logging.info("deleted successfully. node:~p", [nodename])
          {200, n |> Map.from_struct()}

        {:aborted, :not_exist} ->
          Logging.info("not exists. node:~p", [nodename])
          {404, nil}

        {:aborted, e} ->
          Logging.error("unhandled excpetion:~p", [e])
          {500, %{"error" => "unhandled error"}}
      end

    result = %{
      "_meta_" => %{
        @api_duration => Utilities.now() - st_time
      },
      "result" => rtn
    }

    conn |> send_response(status, result)
  end

  def edit_node_config(conn, params) do
    st_time = Utilities.now()

    Logging.debug("called. params:~p", [params])
    node_instance = map_to_node_struct(params)

    {status, rtn} =
      case Repo.edit_node(node_instance) do
        {:aborted, :conflict} ->
          Logging.info("version conflict. node:~p", [node_instance.node_name])
          {409, %{"version" => "conflict"}}

        {:aborted, :not_exist} ->
          Logging.info("node not exists. node:~p", [node_instance.node_name])
          {404, nil}

        {:aborted, :invalid_model} ->
          Logging.info("bad request body to edit node:~p", [node_instance.node_name])
          {400, %{"body" => "invalid_request"}}

        {:aborted, e} ->
          Logging.error("unhandled request. error:~p", [e])
          {500, %{"error" => "unhandled error"}}

        {:atomic, model} ->
          Logging.info("successfully edited. node:~p", [node_instance.node_name])
          {200, model |> Map.from_struct()}
      end

    result = %{
      "_meta_" => %{
        @api_duration => Utilities.now() - st_time
      },
      "result" => rtn
    }

    conn |> send_response(status, result)
  end

  def get_node_config(conn, params) do
    st_time = Utilities.now()

    nodename = params["nodename"]

    {status, rtn} =
      case Repo.get_node(nodename) do
        nil ->
          Logging.info("not found. nodename:~p", [nodename])
          {404, nil}

        db_instance when is_map(db_instance) ->
          {200, db_instance |> Map.from_struct()}

        e ->
          Logging.error("unhandled response:~p", [e])
          {500, %{"error" => "unhandled response"}}
      end

    result = %{
      "_meta_" => %{
        @api_duration => Utilities.now() - st_time
      },
      "result" => rtn
    }

    conn |> send_response(status, result)
  end

  def get_all_nodes_config(conn, params) do
    st_time = Utilities.now()
    Logging.debug("called. params:~p", [params])

    {status, rtn} =
      case Repo.list_all_nodes() do
        l when is_list(l) ->
          {200, l |> Enum.map(fn x -> x |> Map.from_struct() end)}

        e ->
          Logging.error("unhandled result:~p", [e])
          {500, "unhandled result"}
      end

    result = %{
      "_meta_" => %{
        @api_duration => Utilities.now() - st_time
      },
      "result" => rtn
    }

    conn |> send_response(status, result)
  end

  def brief_actives(conn, _params) do
    st_time = Utilities.now

    active_nodes = Utilities.all_active_nodes()

    brief_data = active_nodes
    |> Enum.map(fn x ->
      case :rpc.block_call(x, NodeAdmin, :brief_node_info, [], 5_000) do
        {:badrpc, _} -> nil
        v -> v
      end
    end)

    fin = Enum.zip(active_nodes, brief_data) |> Map.new
    rtn = %{
      "_meta_" => %{
        "duration" => Utilities.now() - st_time
      },
      "result" => fin
    }
    conn|> send_response(200, rtn)
  end
end
