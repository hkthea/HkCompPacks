unit PeekingChromium;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  uCEFChromium, uCEFResponseFilter, uCEFRequest, uCEFInterfaces, uCEFTypes;

type
  TMyResponseFilter=class;

  TOnFilterComplete=procedure(AFilter:TMyResponseFilter;
    aStream:TMemoryStream)of object;
  { TMyResponseFilter }

  TMyResponseFilter=class
  private
    FFilteredUrl: String;
    //FComplete: Boolean;
    fFilter:TCustomResponseFilter;
    FInit: Boolean;
    FmimeType: String;
    FonFilterComplete: TOnFilterComplete;
    FResourceSize: Integer;
    procedure DoFilter(Sender: TObject; data_in: Pointer;
      data_in_size: NativeUInt; var data_in_read: NativeUInt;
      data_out: Pointer; data_out_size: NativeUInt;
      var data_out_written: NativeUInt; var aResult: TCefResponseFilterStatus);
    procedure SetComplete(AValue: Boolean);
    procedure SetFilteredUrl(AValue: String);
    procedure SetInit(AValue: Boolean);
    procedure SetmimeType(AValue: String);
    procedure SetonFilterComplete(AValue: TOnFilterComplete);
    procedure SetResourceSize(AValue: Integer);
  protected
    FStream:TMemoryStream;
    //FFilterUri:String;
    //FRequest:tcefreq
    FComplete:Boolean;
    function InitFilter: Boolean;
    function Filter(data_in: Pointer; data_in_size: NativeUInt;
      var data_in_read: NativeUInt; data_out: Pointer;
      data_out_size: NativeUInt; var data_out_written: NativeUInt
      ): TCefResponseFilterStatus;
    procedure doFilterComplete();
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute();
    property ResourceSize:Integer read FResourceSize write SetResourceSize;
    property Init:Boolean read FInit write SetInit;
    property Complete:Boolean read FComplete write SetComplete;
    property onFilterComplete:TOnFilterComplete read FonFilterComplete write SetonFilterComplete;
    property FilteredUrl:String read FFilteredUrl write SetFilteredUrl;
    property mimeType:String read FmimeType write SetmimeType;
    property ResponseFilter:TCustomResponseFilter read fFilter;
  end;

  { TPeekingChromium }

  TPeekingChromium = class(TChromium)
  private
    FonFilterCompleted: TOnFilterComplete;
    //fFilter:TCustomResponseFilter;
    //fStream:TMemoryStream;
    FResponseFilterList:TList;
    procedure SetonFilterCompleted(AValue: TOnFilterComplete);
    procedure checkResponse(aResponse:ICefResponse; aFilter:TMyResponseFilter);
  protected
    function GetMyResponseFilter(aURL:String):TMyResponseFilter;
    function doOnBeforeResourceLoad(const aBrowser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const callback: ICefRequestCallback
      ): TCefReturnValue; override;
    function doOnResourceResponse(const aBrowser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest;
      const response: ICefResponse): Boolean; override;
    procedure doOnResourceLoadComplete(const aBrowser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest;
      const response: ICefResponse; status: TCefUrlRequestStatus;
      receivedContentLength: Int64); override;
  public
    constructor Create(AOwner: TComponent); override;
    function AddFilter(aUrl:String):Integer;
    procedure GetResourceResponseFilter(const aBrowser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest;
      const response: ICefResponse; var aResponseFilter: ICefResponseFilter);
  published
    property onFilterCompleted:TOnFilterComplete read FonFilterCompleted write SetonFilterCompleted;
  end;

procedure Register;

implementation

uses uregexpr, Math, uCEFStringMultimap;

procedure Register;
begin
  RegisterComponents('HKCompPacks',[TPeekingChromium]);
end;

{ TMyResponseFilter }

procedure TMyResponseFilter.SetResourceSize(AValue: Integer);
begin
  if FResourceSize=AValue then Exit;
  FResourceSize:=AValue;
end;

procedure TMyResponseFilter.SetComplete(AValue: Boolean);
begin
  if FComplete=AValue then Exit;
  FComplete:=AValue;
end;

procedure TMyResponseFilter.DoFilter(Sender: TObject; data_in: Pointer;
  data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer;
  data_out_size: NativeUInt; var data_out_written: NativeUInt;
  var aResult: TCefResponseFilterStatus);
begin
  Filter(data_in, data_in_size, data_in_read, data_out, data_out_size, data_out_written);
end;

procedure TMyResponseFilter.SetFilteredUrl(AValue: String);
begin
  if FFilteredUrl=AValue then Exit;
  FFilteredUrl:=AValue;
end;

procedure TMyResponseFilter.SetInit(AValue: Boolean);
begin
  if FInit=AValue then Exit;
  FInit:=AValue;
end;

procedure TMyResponseFilter.SetmimeType(AValue: String);
begin
  if FmimeType=AValue then Exit;
  FmimeType:=AValue;
end;

procedure TMyResponseFilter.SetonFilterComplete(AValue: TOnFilterComplete);
begin
  if FonFilterComplete=AValue then Exit;
  FonFilterComplete:=AValue;
end;

function TMyResponseFilter.InitFilter: Boolean;
begin
  //Result:=inherited InitFilter;
  Result:=True;//fFilter.InitFilter;
end;

function TMyResponseFilter.Filter(data_in: Pointer; data_in_size: NativeUInt;
  var data_in_read: NativeUInt; data_out: Pointer; data_out_size: NativeUInt;
  var data_out_written: NativeUInt): TCefResponseFilterStatus;
begin
  try
    if FInit then
    begin
      if data_in=nil then
      begin
        data_in_read     := 0;
        data_out_written := 0;
        Result          := RESPONSE_FILTER_DONE;
        if (not FComplete) and (FStream.Size>0) and (FStream.Size=FResourceSize) then
        begin
          doFilterComplete();
        end;
      end
      else
      begin
        if data_out<>nil then
        begin
          data_out_written := min(data_in_size, data_out_size);
          if (data_out_written > 0) then
            Move(data_in^, data_out^, data_out_written);
        end;

        if (data_in_size > 0) then
          data_in_read := FStream.Write(data_in^, data_in_size);

        if not(Complete) and (FResourceSize <> -1) and (FResourceSize = FStream.Size) then
        begin
          //FRscCompleted := PostMessage(Handle, STREAM_COPY_COMPLETE, 0, 0);
          FComplete:=True;
          doFilterComplete();
          Result       := RESPONSE_FILTER_DONE;
        end
        else
          Result := RESPONSE_FILTER_NEED_MORE_DATA;
      end;
    end;
  except
  on e : exception do
  begin
    Result := RESPONSE_FILTER_ERROR;
  end;
  end;

end;

procedure TMyResponseFilter.doFilterComplete();
begin
  if Assigned(onFilterComplete) then onFilterComplete(Self, FStream);
end;

constructor TMyResponseFilter.Create;
begin
  //inherited Create;
  FStream:=TMemoryStream.Create;

end;

destructor TMyResponseFilter.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

procedure TMyResponseFilter.Execute();
begin
  Init:=True;
  Complete:=False;
  FStream.Clear;
  fFilter:=TCustomResponseFilter.Create;
  fFilter.OnFilter:=@DoFilter;
end;

{ TPeekingChromium }

procedure TPeekingChromium.SetonFilterCompleted(AValue: TOnFilterComplete);
begin
  if FonFilterCompleted=AValue then Exit;
  FonFilterCompleted:=AValue;
end;

procedure TPeekingChromium.checkResponse(aResponse: ICefResponse;
  aFilter: TMyResponseFilter);
var
  TempContentLength, TempContentEncoding : string;
  TempLen : integer;
begin
  TempContentEncoding := trim(lowercase(aResponse.GetHeaderByName('Content-Encoding')));
  aFilter.mimeType:=aResponse.MimeType;
  if (length(TempContentEncoding) > 0) and (TempContentEncoding <> 'identity') then
  begin
    // We can't use this information because Content-Length has the
    // compressed length but the OnFilter event has uncompressed data.
    aFilter.ResourceSize:=-1;
  end
  else
  begin
    TempContentLength := trim(aResponse.GetHeaderByName('Content-Length'));

    if (length(TempContentLength) > 0) and
       TryStrToInt(TempContentLength, TempLen) and
       (TempLen > 0) then
    begin
      aFilter.ResourceSize:=-1;
    end
  end;
end;

function TPeekingChromium.GetMyResponseFilter(aURL: String): TMyResponseFilter;
var
  aaa, ct: Integer;
  temp: TMyResponseFilter;
  reg:TRegExpr;
  fltUrl: String;
begin
  reg:=TRegExpr.Create;
  reg.ModifierG:=True;
  reg.ModifierI:=True;
  Result:=Nil;
  ct:=FResponseFilterList.Count;
  try
    for aaa:=0 to ct-1 do
    begin
      temp:=TMyResponseFilter(FResponseFilterList[aaa]);
      fltUrl:=temp.FilteredUrl;
      if fltUrl='' then Continue;
      reg.Expression:=fltUrl;
      if reg.Exec(aURL)then
      begin
        Result:=temp;
        Break;
      end;
    end;
  finally
    reg.Free;
  end;
end;

procedure TPeekingChromium.GetResourceResponseFilter(
  const aBrowser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse;
  var aResponseFilter: ICefResponseFilter);
var
  temp: TMyResponseFilter;
  uri: string;
  //tempLn: ustring;
begin
  uri:=request.Url;
  temp:=GetMyResponseFilter(uri);
  if temp<>nil then
  begin
    checkResponse(response, temp);
    temp.Execute();
    aResponseFilter:=temp.ResponseFilter;
  end;
end;

function TPeekingChromium.doOnBeforeResourceLoad(const aBrowser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  const callback: ICefRequestCallback): TCefReturnValue;
var
  TempOldMap, TempNewMap : ICefStringMultimap;
  i : NativeUInt;
  TempReplaced : boolean;
begin
  Result:=inherited doOnBeforeResourceLoad(aBrowser, frame, request, callback);
  try
    // We replace the Accept-Encoding HTTP header to request uncompressed resources.
    // If the server sends uncompressed resources it should be easier to handle the
    // end of the resource reception because we may know its length.

    TempNewMap := TCefStringMultimapOwn.Create;
    TempOldMap := TCefStringMultimapOwn.Create;

    request.GetHeaderMap(TempOldMap);

    TempReplaced := False;
    i := 0;
    while (i < TempOldMap.Size) do
      begin
        if (CompareText(TempOldMap.Key[i], 'Accept-Encoding') = 0) then
          begin
            TempNewMap.Append('Accept-Encoding', 'identity');
            TempReplaced := True;
          end
         else
          TempNewMap.Append(TempOldMap.Key[i], TempOldMap.Value[i]);

        inc(i);
      end;

    if not(TempReplaced) then TempNewMap.Append('Accept-Encoding', 'identity');
    request.SetHeaderMap(TempNewMap);
  finally
    TempNewMap := nil;
    TempOldMap := nil;
  end;
end;

function TPeekingChromium.doOnResourceResponse(const aBrowser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  const response: ICefResponse): Boolean;
var
  temp: TMyResponseFilter;
  uri: string;
begin
  uri:=request.Url;
  temp:=GetMyResponseFilter(uri);
  if temp<>nil then
  begin
    checkResponse(response, temp);
    temp.mimeType:=response.MimeType;
  end;
  Result:=inherited doOnResourceResponse(browser, frame, request, response);
end;

procedure TPeekingChromium.doOnResourceLoadComplete(
  const aBrowser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse;
  status: TCefUrlRequestStatus; receivedContentLength: Int64);
var
  temp: TMyResponseFilter;
  uri: ustring;
begin
  uri:=request.Url;
  temp:=GetMyResponseFilter(uri);
  if temp<>nil then
  begin
    //checkResponse(response, temp);
    if not temp.Complete then
    begin
      temp.Complete:=true;
      temp.doFilterComplete();
    end;
  end;
  inherited doOnResourceLoadComplete(browser, frame, request, response, status,
    receivedContentLength);
end;

constructor TPeekingChromium.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResponseFilterList:=TList.Create;
end;

function TPeekingChromium.AddFilter(aUrl: String): Integer;
var aaa:Integer;
  Found: Boolean;
  temp: TMyResponseFilter;
begin
  Result:=-1;
  if aUrl='' then Exit;
  Found:=false;
  for aaa:=0 to FResponseFilterList.Count-1 do
  begin
    if TMyResponseFilter(FResponseFilterList[aaa]).FilteredUrl=aUrl then
    begin
      Found:=True;
      break;
    end;
  end;
  if NOT Found then
  begin
    temp:=TMyResponseFilter.Create;
    temp.FilteredUrl:=aUrl;
    temp.onFilterComplete:=onFilterCompleted;
    FResponseFilterList.Add(temp);
  end;
end;

end.
