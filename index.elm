edit = True

personByPosition: List (String, Maybe {name: String, date: String, record: String})
personByPosition =
  __jsEval__ <| Maybe.withDefault "[]" <| fs.read "positions.json"
  
personByPositionDict = Dict.fromList personByPosition

removeSecondName given_name =
  case Regex.extract """^(\S+)\b""" given_name of
    Just [x] -> x
    _ -> given_name

buildpic content =
  <div class="aroundcrop"><div class="crop">@content</div></div>
    
getInfo position =
  Dict.get position personByPositionDict
  |> Maybe.andThen identity
  |> Maybe.map (\{name, date, record} ->
    let (name, pic) =
          case Regex.extract "^(.*),(.*)$" name of
            Just [last, given] ->
              let [trimmedLast, trimmedGiven] = List.map String.trim [last, given]
                  imgname = trimmedGiven + " " + trimmedLast
                  imgpath = """pictures/@(imgname).jpg"""
                  displayName = removeSecondName trimmedGiven + " " + removeSecondName trimmedLast
              in if fs.isfile imgpath then
                (displayName, buildpic <img class="thumbnail" src=@imgpath>)
              else
                (displayName, buildpic <div class="nopic"></div>)
            _ ->
                (name,  buildpic <div class="nopic"></div>)
    in
      {name=name, date=date, pic=pic}
  )

renamings =
  fs.read "renamings.txt"
  |> Maybe.withDefaultReplace (freeze "[\n]")
  |> evaluate

-- If we rename a position visually, the renamining will be stored in the variable "renamings"
displayPosition position = 
  listDict.get position renamings
  |> Maybe.withDefaultReplace (freeze position)
  |> (\posForDisplay ->
    let (posForDisplay2, title, class) =
          if Dict.get position personByPositionDict /= Nothing then
            (posForDisplay, position + " - can be renamed", "position", [])
          else
            let suggestions = List.filterMap (\(existingPosition, _) ->
                    if Regex.matchIn position existingPosition then
                      Just existingPosition
                    else
                      Nothing
                  ) personByPosition
                suggestion =
                  "Position not found. Remove some characters or click on suggestions."
                buttons = 
                  List.map (\suggestion ->
                    Html.button suggestion "Is this the correct position?"
                      (post,    renamings) (\_ ->
                      (suggestion, renamings ++ [(suggestion, position)]))
                  ) suggestions
            in
            (pos, suggestion, "post notfound")
            (position, suggestion, "position notfound", buttons)
    in
    [<span class=@class title=@title>@posForDisplay2</span>] ++ buttons
  )

type OrgChart = Leader String (List OrgChart)

orgCharts: List Leader
orgCharts = 
  fs.read "orgCharts.txt"
  |> Maybe.withDefaultReplace (freeze "[\n]")
  |> evaluate

orgCharts = orgCharts ++ missing orgCharts
  
missing orgCharts =
  let gatherPositions (Leader c o) =
        c :: List.concatMap gatherPositions o
      printedPositions =
        List.concatMap (\(_, _, _, lds) ->
          List.concatMap gatherPositions lds) orgCharts
      positionIsPrinted c =
        List.any (== c) printedPositions
  in
  List.filterMap (\(position, info) ->
    if positionIsPrinted position then Nothing else
      if info == Nothing then Nothing else Just position
  ) personByPosition
  |> List.map (\position ->
    (position, [], [["dontbreakafter", "true"]], [Leader position []])
  )
  
renderOrgChart istop nosiblings orgChart =
  let (Leader position ministers) = orgChart in
  let childNoSiblings = List.length ministers <= 1 in
  let leader =
        case getInfo position of
          Just {name, date, pic} ->
            <div class="leader">@pic<div class="picinfo">@displayPosition(position)<br>@name<br>@date</div></div>
          _ ->
            <div class="leader vacant">@( buildpic <span></span>)<div class="picinfo">@displayPosition(position)<br>Vacant</div></div>
      children =
        Html.div [] [["class", "leaderchildren"]] (List.map (renderOrgChart False childNoSiblings) ministers)
      editactions =
        if edit then
          [Html.button "+" "Add a child" ministers (\x -> x ++ [Leader "TODO" []])] ++ 
          (if List.length ministers >= 1 then 
          [Html.button "-" "Remove children" ministers (\x -> [])]
          else [])
        else []
  in
  <div class=@(
        "orgChart" +
        (if istop then " top" else "") +
        (if nosiblings then " nosiblings" else "") +
        (if childNoSiblings then " left" else "") +
        (if List.length ministers == 0 then " nochildren" else ""))
   >@([leader] ++ editactions ++ [children
  ])</div>
  
renderTopOrgChart (name, style, attrs, orgCharts) =
  Html.div style (["class","top-orgChart"]::attrs) (
    <h1>@name</h1> ::
    (List.map (renderOrgChart True True) orgCharts))

<html><head></head><body>
<style>
span.helper {
    display: inline-block;
    height: 100%;
    vertical-align: middle;
}

span.helper + img {
    vertical-align: middle;
}
span.position {
  white-space: pre;
}
span.position.notfound {
  color: red;
  font-weight: bold;
}
body {
  font-family: sans-serif;
}
.leader > button {
  margin-top: 1.2em;
}
.orgChart > button {
  position: absolute;
  top: 100px;
  z-index: 500;
}
.orgChart > button:nth-child(2) {
  left: 2em;
}
div.vacant div.crop {
  border: 1px solid black;
}
div.nopic {
  width: 100%;
  height: 100%;
  background: #AAA;
}
div.orgChart {
  position: relative;
  display: inline-block;
  text-align: center;
  vertical-align: top;
}
div.orgChart.left {
  text-align: left;
}
div.orgChart:not(.nosiblings):first-child:before {
  content : "";
  position: absolute;
  left    : 3.125em;
  top  : -1.25em;
  height  : 0.9375em;
  width   : calc(100% - 3.125em);  /* percentage of the element width*/
  border-top: 2px solid black;
  border-left: 2px solid black;
  margin-top:1.2em; /*move the border below the text*/
}
div.orgChart:not(.nosiblings):last-child:before {
  content : "";
  position: absolute;
  left    : 0px;
  top  : -1.25em;
  height  : 1em;
  width   : 3.125em;  /* percentage of the element width*/
  border-top: 2px solid black;
  border-right: 2px solid black;
  margin-top:1.2em; /*move the border below the text*/
}
div.orgChart:not(.nosiblings):not(:last-child):not(:first-child):before {
  content : "";
  position: absolute;
  left    : 0px;
  top  : -1.25em;
  height  : 1em;
  width   : 100%;  /* percentage of the element width*/
  border-top: 2px solid black;
  margin-top:1.2em; /*move the border below the text*/
}
div.orgChart:not(.nosiblings):not(:last-child):not(:first-child):after {
  content : "";
  position: absolute;
  left    : 3.125em;
  top  : -1.25em;
  height  : 1em;
  width   : 2px;  /* percentage of the element width*/
  border-left: 2px solid black;
  margin-top:1.2em; /*move the border below the text*/
}

div.orgChart.left.nosiblings:not(.top):before {
  content : "";
  position: absolute;
  left: 3.125em;
  top: -2.1875em;
  height: 2em;
  width: 1px;
  border-left: 2px solid black;
  margin-top:1.2em;
}
div.orgChart.nosiblings:not(.left):not(.nochildren) > * > div.aroundcrop:after {
  content : "";
  position: absolute;
  left: 3.125em;
  top: 5.875em;
  height: 1em;
  width: 1px;
  border-left: 2px solid black;
  margin-top:1.2em;
}

div.picinfo {
  margin-top: 2.1875em;
  margin-left: 0.3125em;
  display: inline-block;
  vertical-align: top;
  text-align: left;
  margin-right: 1.25em;
}
div.crop {
  width: 6.25em;
  height: 6.25em;
  border-radius: 3.125em;
  overflow: hidden;
  display: inline-block;
  margin-top: 0.9375em;
  margin-bottom: 0.9375em;
  background: #FFF;
  border: 1px solid #AAA;
}
div.aroundcrop {
  position: relative;
  display: inline-block;
}
img.thumbnail {
  width: 6.25em;
  margin-bottom: -25%;
}
.top-orgChart {
  margin-left: 1cm;
  margin-top: 1cm;
}
</style>
<style type="text/css" media="print">
button, menu, .menumargin, .codepreview {
  display: none !important;
}

@@page {     size: landscape;}

.top-orgChart:not([dontbreakafter=true]) {
  page-break-after: always;
}
</style>
@(List.map renderTopOrgChart orgCharts)
</body></html>