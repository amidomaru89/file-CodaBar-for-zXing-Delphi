unit ZXing.OneD.CodaBarReader;
{
  * Copyright 2008 ZXing authors
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *      http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.

  * 
  * Implemented by V. Marchenko for Delphi
}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Math,
  ZXing.OneD.OneDReader,
  ZXing.Common.BitArray,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultPoint,
  ZXing.BarcodeFormat,
  ZXing.Common.Detector.MathUtils;

type

  TCodaBarReader = class sealed(TOneDReader)

    private
    class var ALPHABET: TArray<Char>;
    function findStartPattern: Integer;
    function toNarrowWidePattern(position: integer): integer;
    function validatePattern(start: integer):integer;
    function setCounters(const row: IBitArray): boolean;

    function counterAppend(e:integer):boolean;
   
  const
    ALPHABET_STRING: string = '0123456789-$:/.+ABCD';
    MAX_ACCEPTABLE: Double = 2.0;
    PADDING: Double = 1.5;
    MIN_CHARACTER_LENGTH: integer = 3;
   // class var ASTERISK_ENCODING: Integer;

  class var
    STARTEND_ENCODING: TArray<Char>;
    CHARACTER_ENCODINGS: TArray<Integer>;
    counters: TArray<Integer>;
    counterLength: integer;
    decodeRowResult: TStringBuilder;
    usingCheckDigit: boolean;
    extendedMode: boolean;
    function arrayContains(arr: TArray<Char>; key:char ): boolean;

  public
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;



    constructor Create(AUsingCheckDigit: boolean);
    destructor Destroy(); override;




  end;

  implementation

{ CodaBarReader }

constructor TCodaBarReader.Create(AUsingCheckDigit: boolean);
begin
  STARTEND_ENCODING := TArray<Char>.Create('A', 'B', 'C', 'D');
  ALPHABET := ALPHABET_STRING.ToCharArray;
  CHARACTER_ENCODINGS := TArray<Integer>.Create($003, $006, $009, $060, $012, $042, $021, $024, $030, $048, // 0-9
      $00c, $018, $045, $051, $054, $015, $01A, $029, $00B, $00E // -$:/.+ABCD
    );

//  ASTERISK_ENCODING := TCodaBarReader.CHARACTER_ENCODINGS[$27];

  counters := TArray<Integer>.Create();
  SetLength(counters, 80);
  decodeRowResult := TStringBuilder.Create();
  usingCheckDigit := AUsingCheckDigit;


end;

destructor TCodaBarReader.Destroy;
begin
  ALPHABET := nil;
  CHARACTER_ENCODINGS := nil;
  STARTEND_ENCODING := nil;
  counters := nil;
  FreeAndNil(decodeRowResult);
  inherited;
end;

   function TCodaBarReader.arrayContains(arr: TArray<char>; key:char ): boolean;
  var
  c: integer;
  begin

   if (arr <> nil) then begin
      for c:=1 to Length(arr) do
      begin
           if arr[c] = key then
           begin
            Result:= true;
           end;

     end;
   end;
   Result:= false;
 end;



  // Assumes that counters[position] is a bar.
  function TCodaBarReader.toNarrowWidePattern(position: integer): integer;
  var
    ends, maxBar, minBar, i, j, currentCounter, thresholdBar, maxSpace, minSpace: integer;
    thresholdSpace, bitmask, pattern, threshold:integer;
    theCounters: TArray<integer>;
  begin
    ends := position + 7;
    if ends >= counterLength then Result := -1;
    theCounters := counters;
    maxBar := 0;
    minBar := High(Integer);
    j:= position;
    while  j < ends do
    begin
     j:= j+2;
     currentCounter := theCounters[j];
     if (currentCounter < minBar) then  minBar := currentCounter;
     if (currentCounter > maxBar) then  maxBar := currentCounter;

    end;

    thresholdBar := trunc( (minBar + maxBar) / 2);
    maxSpace := 0;
    minSpace := High(Integer);

    j := position+1;
    while j < ends do
    begin
    j:= j+2;
    currentCounter := theCounters[j];
    if (currentCounter < minSpace) then minSpace := currentCounter;
    if (currentCounter > maxSpace) then maxSpace := currentCounter;

    end;
    thresholdSpace := trunc((minSpace + maxSpace) / 2);
    bitmask := 1 Shl  7;
    pattern := 0;
    I := 0;
    while i < 7 do begin
    i:= i+1;
    if ((i and 1) = 0) then threshold:= thresholdBar else threshold:=thresholdSpace;


    bitmask:= bitmask Shr 1;
    if (theCounters[position + i] > threshold) then pattern := pattern or bitmask;

    end;


    i := 0;
    while i < length(CHARACTER_ENCODINGS) do
   begin
      if CHARACTER_ENCODINGS[i] = pattern then  Result := i;
      i:= i+1;
   end;

    //Result := -1;

  end;





function TCodaBarReader.findStartPattern():integer;
var
i, j, charOffset, patternSize: integer;
begin
i := 1;
    while i < counterLength do
     begin
     i := i + 2;
     charOffset := toNarrowWidePattern(i);
      if ((charOffset <> -1) AND (arrayContains(STARTEND_ENCODING, ALPHABET[charOffset]))) then
      begin
        // Look for whitespace before start pattern, >= 50% of width of start pattern
        // We make an exception if the whitespace is the first element.
        patternSize := 0;
        j := i;
        while j < (i + 7) do begin
         j := j+ 1;
          patternSize := patternSize + counters[j];
        end;
        if ((i = 1) OR (counters[i - 1] >= (patternSize / 2))) then Result := i;
    end;
  end;
  //Result:= 0;
end;


function TCodaBarReader.validatePattern(start:integer): integer;
var
 sizes, counts: TArray<Integer>;
 ends, pos, i, j, pattern, size, category, charack: integer;
 maxes, mins: TArray<Double>;

begin
  sizes := TArray<Integer>.create(0, 0, 0, 0);
  counts := TArray<Integer>.create(0, 0, 0, 0);
  ends := decodeRowResult.length - 1;
  // We break out of this loop in the middle, in order to handle
  // inter-character spaces properly.
  pos := start;
  i := 0;

  while true do
  begin
    i:=i+1;
            //   CHARACTER_ENCODINGS[decodeRowResult.charAt(i)];
    if decodeRowResult[i] <> '-' then  begin
       pattern := CHARACTER_ENCODINGS[StrToInt(decodeRowResult[i])];
       j:= 6;
      while j >= 0 do
      begin
         j:= j-1;
         // Even j = bars, while odd j = spaces. Categories 2 and 3 are for
          // long stripes, while 0 and 1 are for short stripes.
          category := (j and 1) + (pattern and 1) * 2;
          sizes[category] := sizes[category] + counters[pos + j];
          counts[category]:= counts[category]+ 1;
          pattern := pattern Shr 1;
      end;

      if i >= ends then break;

      // We ignore the inter-character space - it could be of any size.
      pos := pos + 8;
    end;

  end;

  // Calculate our allowable size thresholds using fixed-point math.

    SetLength(maxes, 4);
    SetLength(mins, 4);

    // Define the threshold of acceptability to be the midpoint between the
    // average small stripe and the average large stripe. No stripe lengths
    // should be on the "wrong" side of that line.


    // выбивает ошибку деления на ноль
    {i := 0;
    while i < 2 do
    begin
     i:= i+1;
      mins[i] := 0.0;  // Accept arbitrarily small "short" stripes.
      mins[i + 2] := (sizes[i] / counts[i] + sizes[i + 2] / counts[i + 2]) / 2.0;
      maxes[i] := mins[i + 2];
      maxes[i + 2] := (sizes[i + 2] * MAX_ACCEPTABLE + PADDING) / counts[i + 2];
    end; }

    // Now verify that all of the stripes are within the thresholds.
    pos := start;
    i := 0;
    while true do
    begin
      i:= i+1;
       if decodeRowResult[i] <> '-' then  begin
         pattern := CHARACTER_ENCODINGS[StrToInt(decodeRowResult[i])];

       j := 6;
      while j >= 0 do
      begin
      j:= j-1;
       // Even j = bars, while odd j = spaces. Categories 2 and 3 are for
        // long stripes, while 0 and 1 are for short stripes.
        category := (j and 1) + (pattern and 1) * 2;
        size := counters[pos + j];
        if ((size < mins[category]) OR (size > maxes[category])) then
        begin
         Result:=0;
         exit
         end;
        pattern := pattern Shr 1;
      end;

      if i >= ends then  break;
      pos := pos + 8;
       end;      //   CHARACTER_ENCODINGS[decodeRowResult.charAt(i)];

    end;



end;

  {**
   * Records the size of all runs of white and black pixels, starting with white.
   * This is just like recordPattern, except it records all the counters, and
   * uses our builtin "counters" member for storage.
   * @param row row to count from
   *}
 function TCodaBarReader.setCounters(const row: IBitArray): boolean;
 var
 i, ends, count:integer;
 isWhite: boolean;
 begin
    counterLength := 0;
    // Start from the first white bit.
    i:= row.getNextUnset(0) ;//. getNextUnset(0);
    ends := row.Size;
    if i >= ends then begin
     Result:=false;
     exit
     end;
    isWhite := true;
    count := 0;
    while i < ends do begin
      if (row[i] <> isWhite) then
        count:= count + 1
      else begin
        counterAppend(count);
        count := 1;
        isWhite := false;
      end;

     i:= i+1;
    end;

    counterAppend(count);
 end;






  function TCodaBarReader.counterAppend(e:integer):boolean;
  var
  temp: TArray<Integer>;
  begin
  counters[counterLength] := e;
  counterLength:= counterLength+1;
  if (counterLength >= length(counters)) then
    begin
         SetLength(temp, counterLength * 2);

          System.CopyArray(counters, temp, 0, counterLength);       //arraycopy(counters, 0, temp, 0, counterLength);
          counters := temp;
    end;


  end;









function TCodaBarReader.decodeRow(const rowNumber: Integer; const row: IBitArray;
const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  startOffset, nextStart, charOffset, trailingWhitespace, lastPatternSize, runningCount, i: integer;
  startchar, endchar: Char;
  left, right: Double;
  resultPoints: TArray<IResultPoint>;
  temps: string;
begin

for i := 0 to Length(counters)-1 do counters[i] := 0;


    setCounters(row);
     startOffset := 0;//findStartPattern();
    nextStart := startOffset;

    //decodeRowResult.Clear; // .setLength(0);


    //for I := 0 to counterLength do  temps:= temps + counters[i].ToString + ' ';




    for I := 0 to counterLength do decodeRowResult.append(toNarrowWidePattern(i));


   { while (nextStart < counterLength) do
    begin
     charOffset := toNarrowWidePattern(nextStart);

     decodeRowResult.append(charOffset.ToString);
     nextStart := nextStart + 8;
    end;  }



    repeat
      begin
      charOffset := toNarrowWidePattern(nextStart);
      if charOffset = -1 then begin
      Result:= nil;
      exit
      end;
      
      // Hack: We store the position in the alphabet table into a
      // StringBuilder, so that we can access the decoded patterns in
      // validatePattern. We'll translate to the actual characters later.
      decodeRowResult.append(charOffset);

      nextStart := nextStart + 8;
      // Stop as soon as we see the end character.
      if ((decodeRowResult.Length > 1) AND (arrayContains(STARTEND_ENCODING, ALPHABET[charOffset]))) then break;
    end until (nextStart < counterLength); // no fixed end pattern so keep on reading while data is available





    // Look for whitespace after pattern:
    trailingWhitespace := counters[nextStart - 1];
    lastPatternSize := 0;
    for i:= -8 to -1 do begin
     lastPatternSize := lastPatternSize + counters[nextStart + i];
    end;

   Result := TReadResult.Create(lastPatternSize.ToString, nil, nil, TBarcodeFormat.CODABAR);

    {
    // We need to see whitespace equal to 50% of the last pattern size,
    // otherwise this is probably a false positive. The exception is if we are
    // at the end of the row. (I.e. the barcode barely fits.)
    if ((nextStart < counterLength) AND (trailingWhitespace < (lastPatternSize / 2))) then
    begin
      Result:=nil;
      exit
      end;

    }




    validatePattern(startOffset);



    // Translate character table offsets to actual characters.
    for i:= 0 to decodeRowResult.length do begin
    //  decodeRowResult.setCharAt(i, ALPHABET[decodeRowResult.charAt(i)]);
    // но это может быть нихрена не правильно
    if decodeRowResult[i] <> '-' then
    decodeRowResult.Chars[i]:= ALPHABET[StrToInt(decodeRowResult[i])];
    end;
    Result := TReadResult.Create(decodeRowResult.ToString, nil, nil, TBarcodeFormat.CODABAR);
   {
    // Ensure a valid start and end character
    startchar := decodeRowResult[0];
    if NOT (arrayContains(STARTEND_ENCODING, startchar)) then begin
      Result:=nil;
      exit
      end;
    
    endchar := decodeRowResult[decodeRowResult.Length-1];
    if NOT (arrayContains(STARTEND_ENCODING, endchar)) then begin
      Result:=nil;
      exit
      end;
    

    // remove stop/start characters character and check if a long enough string is contained
    if (decodeRowResult.Length <= MIN_CHARACTER_LENGTH) then begin
      // Almost surely a false positive ( start + stop + at least 1 character)
      Result:=nil;
      exit
    end;

    if ((hints = nil) OR NOT (hints.containsKey(TDecodeHintType.RETURN_CODABAR_START_END))) then begin
      decodeRowResult.Remove(decodeRowResult.Length - 1, 1);
      decodeRowResult.Remove(0,1);
    end;

    runningCount := 0;
    i := 0;
    while i < startOffset do begin
      i:= i+1;
      runningCount := runningCount + counters[i];
    end;
    left := runningCount;
    i := startOffset;
    while i < (nextStart - 1) do
      begin
      i:= i+1;
      runningCount := runningCount + counters[i];
    end;
    right := runningCount;


       }



    //Result := TReadResult.Create(decodeRowResult.ToString ,nil, nil, TBarcodeFormat.CODABAR);
 
end;












 end.


