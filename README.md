<div align="center">

## Adjust a GIF's Hue/Sat/Lum


</div>

### Description

This code adjusts the Hue, Saturation and Luminosity of any gif file on the fly! No component needed, it's just asp code.

live demo:

http://www.cwolves.com/testing/coloradj.html

Live site using this:

http://www.cwolves.com/home.asp?hue=0&sat=0&lum=0&con=0&red=true&green=true&blue=true&invert=false

play with all the querystring values in that link.
 
### More Info
 
querystring inputs:

img (image path)

hue (-180 --> 180)

sat (-100 --> 100)

lum (-100 --> 100)

your new gif


<span>             |<span>
---                |---
**Submitted On**   |
**By**             |[Mark Kahn](https://github.com/Planet-Source-Code/PSCIndex/blob/master/ByAuthor/mark-kahn.md)
**Level**          |Advanced
**User Rating**    |5.0 (30 globes from 6 users)
**Compatibility**  |ASP \(Active Server Pages\)
**Category**       |[Graphics/ Sound](https://github.com/Planet-Source-Code/PSCIndex/blob/master/ByCategory/graphics-sound__4-15.md)
**World**          |[ASP / VbScript](https://github.com/Planet-Source-Code/PSCIndex/blob/master/ByWorld/asp-vbscript.md)
**Archive File**   |[](https://github.com/Planet-Source-Code/mark-kahn-adjust-a-gif-s-hue-sat-lum__4-8626/archive/master.zip)





### Source Code

```
<%
response.contenttype	= "image/gif"
src = request.querystring("Img")
Hue = request.querystring("Hue")
Sat = request.querystring("Sat")
Lum = request.querystring("Lum")
function dofix(val)
	if val > 239 then
		val = val - 239
		val = dofix(val)
	elseif val < 0 then
		val = val + 239
		val = dofix(val)
	end if
	dofix = val
end function
function dofix2(val)
	if val > 239 then
		dofix2 = 239
	elseif val < 0 then
		dofix2 = 0
	else
		dofix2 = val
	end if
end function
function hexval(char)
	select case char
	case "0": hexval	= 0
	case "1": hexval	= 1
	case "2": hexval	= 2
	case "3": hexval	= 3
	case "4": hexval	= 4
	case "5": hexval	= 5
	case "6": hexval	= 6
	case "7": hexval	= 7
	case "8": hexval	= 8
	case "9": hexval	= 9
	case "A": hexval	= 10
	case "B": hexval	= 11
	case "C": hexval	= 12
	case "D": hexval	= 13
	case "E": hexval	= 14
	case "F": hexval	= 15
	end select
end function
function Ascii(color)
	out	= ""
	for i = 0 to 2
		this	= mid(color, i*2+1, 2)
		tmp	= 16*hexval(left(this, 1)) + hexval(right(this, 1))
		out	= out & chr(tmp)
	next
	Ascii	= out
end function
Function stb(tString)
 Dim I, B
 For I=1 to len(tString)
 B = B & ChrB(Asc(Mid(tString,I,1)))
 Next
 stb = B
End Function
Function RGBtoHSL(ByVal Red, ByVal Green, ByVal Blue, ByRef Hue, ByRef Sat, ByRef Lum)
	pRed = Red / 255
	pGreen = Green / 255
	pBlue = Blue / 255
	If pRed > pGreen Then
		If pRed > pBlue Then pMax = pRed Else pMax = pBlue
	ElseIf pGreen > pBlue Then
		pMax = pGreen
	Else
		pMax = pBlue
	End If
	If pRed < pGreen Then
		If pRed < pBlue Then pMin = pRed Else pMin = pBlue
	ElseIf pGreen < pBlue Then
		pMin = pGreen
	Else
		pMin = pBlue
	End If
	pLum = (pMax + pMin) / 2
	If pMax = pMin Then
		pSat = 0
		pHue = 0
		Else
			If pLum < 0.5 Then
				pSat = (pMax - pMin) / (pMax + pMin)
			Else
				pSat = (pMax - pMin) / (2 - pMax - pMin)
		End If
		Select Case pMax
			Case pRed
				pHue = (pGreen - pBlue) / (pMax - pMin)
			Case pGreen
				pHue = 2 + (pBlue - pRed) / (pMax - pMin)
			Case pBlue
				pHue = 4 + (pRed - pGreen) / (pMax - pMin)
		End Select
	End If
	Hue = pHue * 239 \ 6
	If Hue < 0 Then Hue = Hue + 240
	Sat = Int(pSat * 239)
	Lum = Int(pLum * 239)
End Function
Function HSLtoRGB(ByRef Red, ByRef Green, ByRef Blue, ByVal Hue, ByVal Sat, ByVal Lum)
	Dim temp3()
	ReDim temp3(3)
	pHue = Hue / 239
	pSat = Sat / 239
	pLum = Lum / 239
	If pSat = 0 Then
		pRed = pLum
		pGreen = pLum
		pBlue = pLum
	Else
		If pLum < 0.5 Then
			temp2 = pLum * (1 + pSat)
		Else
			temp2 = pLum + pSat - pLum * pSat
		End If
		temp1 = 2 * pLum - temp2
		temp3(0) = pHue + 1 / 3
		temp3(1) = pHue
		temp3(2) = pHue - 1 / 3
		For n = 0 To 2
			If temp3(n) < 0 Then temp3(n) = temp3(n) + 1
			If temp3(n) > 1 Then temp3(n) = temp3(n) - 1
			If 6 * temp3(n) < 1 Then
				temp3(n) = temp1 + (temp2 - temp1) * 6 * temp3(n)
			Else
				If 2 * temp3(n) < 1 Then
					temp3(n) = temp2
				Else
					If 3 * temp3(n) < 2 Then temp3(n) = temp1 + (temp2 - temp1) * ((2 / 3) - temp3(n)) * 6 Else temp3(n) = temp1
				End If
			End If
		Next
		pRed = temp3(0)
		pGreen = temp3(1)
		pBlue = temp3(2)
	End If
	Red = Int(pRed * 255)
	Green = Int(pGreen * 255)
	Blue = Int(pBlue * 255)
End Function
if not(src	= "" or Hue	= "") then
	set FSO	= server.createobject("Scripting.FileSystemObject")
	set file	= FSO.OpenTextFile(server.mappath(src), 1, false)
	StartData = file.read(13)
	TableSize = asc(mid(StartData, 11, 1))
	tSize  = 1
	if (TableSize AND 1) = 1 then tSize = tSize + 1
	if (TableSize AND 2) = 2 then tSize = tSize + 2
	if (TableSize AND 4) = 4 then tSize = tSize + 4
	tSize  = 2^tSize
	ColorData = file.read(tSize*3)
	EndData = ""
	do while not file.atendofstream
		EndData = EndData & file.read(100)
	loop
	file.close: set file	= nothing
	set FSO	= nothing
	NewColorData = ""
	for i = 1 to len(ColorData)-2 STEP 3
		R = asc(mid(colordata, i+0, 1))
		G = asc(mid(colordata, i+1, 1))
		B = asc(mid(colordata, i+2, 1))
		H = 0
		L = 0
		S = 0
		call RGBtoHSL(R, G, B, H, S, L)
		H = H + (239/360*Hue)
		if Sat < 0 then S = S + Sat * .01 * (S) else S = S + Sat * .01 * (239-S)
		if Lum < 0 then L = L + Lum * .01 * (L) else L = L + Lum * .01 * (239-L)
		h = dofix(h)
		s = dofix2(s)
		l = dofix2(l)
		call HSLtoRGB(R, G, B, H, S, L)
		NewColorData = NewColorData & chr(R) & chr(G) & chr(B)
	next
	response.binarywrite stb(StartData & NewColorData & EndData)
end if
%>
```

