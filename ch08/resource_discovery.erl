-module(resource_discovery).

-behaviour(gen_server).

-export([start_link/0,
         add_target_resource_type/1,
         add_local_resource/2,
         fetch_resources/1,
         trade_resources/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {target_resource_types,
                local_resource_tuples,
                found_resource_tuples}).

%% APIs
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_target_resource_type(Type) ->
    gen_server:cast(?SERVER, {add_target_resource_type, Type}).

add_local_resource(Type, Resource) ->
    gen_server:cast(?SERVER, {add_local_resource, {Type, Resource}}).

fetch_resources(Type) ->
    gen_server:call(?SERVER, {fetch_resources, Type}).

trade_resources() ->
    gen_server:cast(?SERVER, trade_resources).


%% gen_server callbacks
init([]) ->
    {ok, #state{target_resource_types=[],
                local_resource_tuples=dict:new(),
                found_resource_tuples=dict:new()}}.


handle_call({fetch_resources, Type}, _From, State) ->
    {reply, dict:find(Type, State#state.found_resource_tuples), State}.


handle_cast({add_target_resource_type, Type}, State) ->
    TargetTypes = State#state.target_resource_types,
    NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
    {noreply, State#state{target_resource_types=NewTargetTypes}};

handle_cast({add_local_resource, {Type, Resource}}, State) ->
    ResourceTuples = State#state.local_resource_tuples,
    NewResourceTuples = add_resource(Type, Resource, ResourceTuples),
    {noreply, State#state{local_resource_tuples=NewResourceTuples}};

handle_cast(trade_resources, State) ->
    ResourceTuples = State#state.local_resource_tuples,
    AllNodes = [node() | nodes()],
    lists:foreach(
        fun (Node) ->
            gen_server:cast({?SERVER, Node}, 
                            {trade_resources, {node(), ResourceTuples}})
        end,
        AllNodes),
    {noreply, State};

handle_cast({trade_resources, {ReplyTo, RemoteTuples}}, State) ->
    #state{target_resource_types=TargetTypes,
           local_resource_tuples=LocalTuples,
           found_resource_tuples=FoundTuples} = State,
    FilteredRemoteTuples = resources_for_types(TargetTypes, RemoteTuples),
    NewFoundTuples = add_resource(FilteredRemoteTuples, FoundTuples),
    case ReplyTo of
        noreply ->
            ok;
        _ ->
            gen_server:cast({?SERVER, ReplyTo},
                            {trade_resources, {noreply, LocalTuples}})
    end,
    {noreply, State#state{found_resource_tuples=NewFoundTuples}}.


handle_info(ok = _Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


add_resource([{Type, Resource} | Tail], Dict) ->
    add_resource(Tail, add_resource(Type, Resource, Dict));
add_resource([], Dict) ->
    Dict.

add_resource(Type, Resource, Dict) ->
    case dict:find(Type, Dict) of
        {ok, List} ->
            NewList = [Resource | lists:delete(Resource, List)],
            dict:store(Type, NewList, Dict);
        error ->
            dict:store(Type, [Resource], Dict)
    end.

resources_for_types(Types, ResourceTuples) ->
    Fun =
        fun (Type, Acc) ->
            case dict:find(Type, ResourceTuples) of
                {ok, List} ->
                    [{Type, Resource} || Resource <- List] ++ Acc;
                error ->
                    Acc
            end
        end,
    lists:foldl(Fun, [], Types).