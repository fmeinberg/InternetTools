BeginPackage["InternetTools`", {"Utilities`URLTools`"}]

OpenURL::usage=""
DownloadFile::usage=""
DownloadHyperlinks::usage=""
hexEncode::usage=""
validURLCharacter::usage=""
encodeURL::usage=""
queryString::usage=""
queryURL::usage=""
import::usage=""

Begin["Private`"]

OpenURL[url_] :=
    NotebookLocate[{URL[url],None}]

DownloadFile[file_, dir_: $HomeDirectory] :=
 Block[{}, If[Not@FileExistsQ[dir], CreateDirectory[dir]];
  FetchURL[file, ToFileName[dir, FileNameTake@file]]]

Options[DownloadHyperlinks]= {"FileExtension"->Automatic}

DownloadHyperlinks[url_, targetDir_,patt_:"*",transform_:Identity,opts:OptionsPattern[]] :=
	Block[{}, If[Not@FileExistsQ@targetDir, CreateDirectory[targetDir]];
	(Print@#;
	    With[{target =
	       ToFileName[targetDir,
	        StringReplace[FileNameTake@#, Whitespace -> "-"]]},
	     If[Not@FileExistsQ@target,
	      FetchURL[
	       StringReplace[#, w : WhitespaceCharacter :> hexEncode@w],
	       target]]]) &[transform@#]& /@
	  Union@Select[import[url, {"HTML","Hyperlinks"},opts],
	    StringMatchQ[#, patt, IgnoreCase -> True] &]
	    ]


(* Comes from Rob Raguet-Schonfield's Twitter code  at  http://blog.wolfram.com/2009/04/30/twittering-with-mathematica/ *)

hexEncode[s_String] :=
    StringJoin[
    Riffle[IntegerString[ToCharacterCode[s, "UTF-8"], 16, 2], "%", {1, -2, 2}]
    ]

validURLCharacter = Alternatives@@Flatten@{WordCharacter, Characters["$-_.+!*'(),/"]}

encodeURL[s_String]:=
	StringReplace[s, { "http://" -> "http://", char:Except[validURLCharacter]:>hexEncode[char]}]


queryString[args_List] :=
    StringJoin@@Riffle[StringJoin[
        ToString@First@#, "=", ToString@Last@#]& /@ args, "&"]


queryURL[url_String, args_List] :=
    Module[ {query = queryString[args]},
        If[ StringLength[query] =!= 0,
            url <> "?" <> query,
            url
        ]
    ];

(* Use this while Import is broken on my OS installation *)

Options[import]= {"FileExtension"->Automatic}

import[link_, format_:Automatic,OptionsPattern[]] :=
 Import[FetchURL[link,
   "/Users/fredm/temp/temp" <>
    Switch[OptionValue["FileExtension"], _String,
     OptionValue["FileExtension"], _,
     Switch[#, "", "", _String, "." <> #] &@FileExtension[link]]],
  format /. Automatic:>Sequence[]]

End[];

EndPackage[]