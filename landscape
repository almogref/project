-module(landscape).
-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  handle_event/2, handle_sync_event/3,
  terminate/2, code_change/3, paint_board/1]).

start_link() ->
  wx_object:start_link({local, landscape},landscape, [], []).


init([]) ->
  io:format("~p~n ",[self()]),
  wx:new(),
  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Landscape"),
  Sizer = wxBoxSizer:new(?wxVERTICAL),
  Panel = wxPanel:new(Frame,[{size, {320,320}},
    {style, ?wxFULL_REPAINT_ON_RESIZE}]),
  wxSizer:add(Sizer, Panel, [{proportion, 1},
    {flag, ?wxEXPAND bor ?wxALL},
    {border, 5}]),
  wxFrame:setSizer(Frame, Sizer),
  wxSizer:setSizeHints(Sizer, Frame),
  wxPanel:connect(Panel, paint, [callback]),
  BirdsT = ets:new(birds,[ordered_set]),

  %----- FOR TESTS ---------
  ets:insert(BirdsT,{1,{{6,6},0}}),
  %------------------------

  MainT = ets:new(table,[set]),
  ets:insert(MainT,{size,0}),
  State = #{frame => Frame, panel => Panel, birds => BirdsT, table => MainT},
  wxFrame:show(Frame),
  {Panel, State}.


handle_call({data,List}, _From, State) ->
  updateState(List,State),
  {reply, ok, State};


handle_call({frame}, _From, State) ->
  paint_board(State),
  {reply, ok, State};

handle_call(_, _From, State) ->
  {reply, ok, State}.


handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

handle_event(#wx{}, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.





% Here we update our current state e.g. adding more birds and so on..
updateState(List,State) ->
  #{birds := BirdsT} = State,
  Insert = fun(Bird) ->
      ets:insert(BirdsT,{element(3,Bird),{element(1,Bird),element(2,Bird)}}) end,
  lists:foreach(Insert,List).





% handle the sync events in the graphic class
handle_sync_event(#wx{event=#wxPaint{}},_,State)->
  paint_board(State).


% paint the objects on the wxDC object
paint_board(State) ->
  #{birds := BirdsT} = State,
  ListOfBirds = buildListOfBirds(BirdsT),
  #{panel := Panel} = State,
  ClientDC = wxClientDC:new(Panel),
  BufferDC = wxBufferedDC:new(ClientDC),
  View = wxImage:new("land_1.jpg"),
  Bitmap_View = wxBitmap:new(View),
  wxDC:drawBitmap(BufferDC, Bitmap_View, {0,0}),
  Draw = fun(Bird) ->
    BirdImage = wxImage:new("bird_3.png"),
    Coordinates = element(1,element(2,Bird)),
    Angle = element(2,element(2,Bird)),
    BirdImageRotate = wxImage:rotate(BirdImage,Angle,Coordinates),
    BirdImageRotateBit = wxBitmap:new(BirdImageRotate),
    wxDC:drawBitmap(BufferDC, BirdImageRotateBit, Coordinates)
         end,
  lists:foreach(Draw,ListOfBirds),
  wxBufferedDC:destroy(BufferDC),
  wxClientDC:destroy(ClientDC).

% Builds the new list of our birds to paint
buildListOfBirds(BirdsT) ->
  Func = fun(Bird,Acc) ->
        [Bird]++Acc end,
  ets:foldl(Func,[],BirdsT).


terminate(_Reason,_)->
  wx:destroy().

