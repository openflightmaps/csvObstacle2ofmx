Imports System
Imports System.Reflection
Imports System.Security.Cryptography
Imports System.Text
Imports System.Xml

Module Program

    Structure DataFormatStruct
        Dim Datatype As String
        Dim RootElement As String
        Dim Attributes As Object
        Dim RootAttribute As String
        Dim AttributesPointer1 As Object
        Dim AttributesPointer2 As Object
        Dim AttributesPointer3 As Object
        Dim AttributesPointer4 As Object
        Dim AttributesPointer5 As Object
        Dim region As String
        Dim isNewEntity As Boolean
        Dim importMode As String
        Dim crossLoadSource As String
    End Structure

    ' Obstacles
    Structure obstacleGroupStruct
        Dim txtName As String
        Dim valGeoAccuracy As Double
        Dim uomGeoAccuracy As String
        Dim valElevAccuracy As Double
        Dim uomElevAccuracy As String
        Dim txtRmk As String
        Dim origin As String
    End Structure

    Structure obstacleStruct
        Dim txtName As String
        Dim codeType As String
        Dim codeGroup As Boolean
        Dim codeLgt As Boolean
        Dim codeMarking As Boolean
        Dim txtDescrLgt As String
        Dim txtDescrMarking As String
        Dim valElev As Double
        Dim valHgt As Double
        Dim uomDistVer As String
        Dim codeHgtAccuracy As Boolean
        Dim valRadius As Double
        Dim uomRadius As String
        Dim codeLinkType As String
        Dim datetimeValidWef As String
        Dim datetimeValidTil As String
        Dim txtRmk As String
        Dim geoLat As Double
        Dim geoLong As Double
        Dim ObsUidLink As Double
    End Structure
    Dim ReplaceFrom As String = ","
    Dim ReplaceTo As String = "."
    Sub Main(args As String())

        ' dot / comma
        If Globalization.NumberFormatInfo.CurrentInfo.NumberDecimalSeparator = "," Then
            ReplaceFrom = "."
            ReplaceTo = ","
        End If
        Dim Filepath As String = ""
        For i As Short = 0 To args.Count - 1
            If args(i) = "--f" Then
                Filepath = args(i + 1)
                Console.WriteLine("will convert: " & Filepath)
            End If

        Next

        If Filepath = "" Then
            Console.WriteLine("please give parameter -f -> input filepath!")
        End If
        parseCsv2OFMX(Filepath)

    End Sub




    Dim minX As Double = -200
    Dim minY As Double = -200
    Dim maxX As Double = -200
    Dim maxY As Double = -200

    Dim TileX(360) As List(Of String)
    Dim TileY(180) As List(Of String)


    Function parseCsv2OFMX(file As String) As DataFormatStruct()
        Dim str = System.IO.File.ReadAllText(file)
        Dim lines = str.Split({CType(vbNewLine, Char), CType(vbLf, Char)})

        Dim headLine() As String = lines(0).Split({CType(";", Char), CType(",", Char)})
        Dim groupId = getIdOfHeader("codeGroupId", headLine)

        Dim obsId As Double = getIdOfHeader("codeId", headLine)

        If groupId = -1 Then
            groupId = getIdOfHeader("codeId", headLine)
        End If

        If obsId = -1 Then obsId = groupId


        Dim codeType As Double = getIdOfHeader("codeType", headLine)

        Dim name As Double = getIdOfHeader("txtName", headLine)
        Dim lighted As Double = getIdOfHeader("codeLgt", headLine)
        Dim marked As Double = getIdOfHeader("codeMarking", headLine)

        Dim txtDescrLgt As Double = getIdOfHeader("txtDescrLgt", headLine)
        Dim txtDescrMarking As Double = getIdOfHeader("txtDescrMarking", headLine)

        Dim latitude As Double = getIdOfHeader("geoLat", headLine)
        Dim longitutde As Double = getIdOfHeader("geoLong", headLine)

        Dim valGeoAccuracy As Double = getIdOfHeader("valGeoAccuracy", headLine)
        Dim uomGeoAccuracy As Double = getIdOfHeader("uomGeoAccuracy", headLine)
        Dim valElev As Double = getIdOfHeader("valElev", headLine)
        Dim valElevAccuracy As Double = getIdOfHeader("valElevAccuracy", headLine)

        Dim valHgt As Double = getIdOfHeader("valHgt", headLine)
        Dim verticalPrecision As Double = getIdOfHeader("codeHgtAccuracy", headLine)
        Dim uomDistVer As Double = getIdOfHeader("uomDistVer", headLine)

        Dim valRadius As Double = getIdOfHeader("valRadius", headLine)
        Dim uomRadius As Double = getIdOfHeader("uomRadius", headLine)

        Dim txtGroupName As Double = getIdOfHeader("txtGroupName", headLine)
        Dim codeLinkedToId As Double = getIdOfHeader("codeLinkedToId", headLine)
        Dim linktype As Double = getIdOfHeader("codeLinkType", headLine)

        Dim datetimeValidWef As Double = getIdOfHeader("datetimeValidWef", headLine)
        Dim datetimeValidTil As Double = getIdOfHeader("datetimeValidTil", headLine)

        Dim txtRmk As Double = getIdOfHeader("txtRmk", headLine)
        Dim source As Double = getIdOfHeader("source", headLine)

        Dim retLst As New List(Of DataFormatStruct)

        Dim idCntr As Long = 5017342

        Dim midCntr As Short = 0

        For i As Long = 1 To lines.Length - 1

            Dim obstacleGroup As New obstacleGroupStruct
            lines(i) = lines(i).Replace(vbNewLine, " ").Replace("""", "")
            Dim val = lines(i).Split({CType(";", Char)})

            Try

                If val.Length > 10 Then
                    If valGeoAccuracy <> -1 Then obstacleGroup.valGeoAccuracy = val(valGeoAccuracy)
                    If uomGeoAccuracy <> -1 Then obstacleGroup.uomGeoAccuracy = val(uomGeoAccuracy)

                    If valElevAccuracy <> -1 Then obstacleGroup.valElevAccuracy = val(valElevAccuracy)
                    If uomDistVer <> -1 Then obstacleGroup.uomElevAccuracy = val(uomDistVer)

                    obstacleGroup.txtRmk = val(txtRmk)
                        obstacleGroup.txtName = val(name)
                        obstacleGroup.origin = val(source)


                        Dim id As Long = -1
                        Try
                            If val(groupId) <> "" Then id = val(groupId) + idCntr
                        Catch ex As Exception

                        End Try


                        ' find all childs
                        Dim obstacleLst As New List(Of obstacleStruct)
                        Dim linkId As Short = 1
                        Dim valL() As String = {""}

                        For l As Long = 1 To lines.Length - 1
                            lines(l) = lines(l).Replace("""", "")
                        valL = lines(l).Split({CType(";", Char)})
                        Try

                                If valL.Length > 1 Then

                                    If valL(groupId) = "" Then valL(groupId) = -1



                                Dim cId As Long

                                Try
                                    cId = CType(valL(groupId) + idCntr, Long)
                                Catch ex As Exception

                                End Try

                                If id = cId Or id = -1 Then

                                    If id = -1 Then
                                        valL = val
                                    End If

                                    Dim obstacle As New obstacleStruct
                                    If (valL(lighted) = "Y") Then obstacle.codeLgt = True
                                    If (valL(marked) = "Y") Then obstacle.codeMarking = True

                                    If txtDescrLgt <> -1 Then obstacle.txtDescrLgt = valL(txtDescrLgt)
                                    obstacle.txtDescrMarking = valL(txtDescrMarking)
                                    obstacle.valElev = valL(valElev)
                                    obstacle.valHgt = valL(valHgt)

                                    obstacle.uomDistVer = valL(uomDistVer)
                                    If (valL(verticalPrecision) = "Y") Then obstacle.codeHgtAccuracy = True
                                    obstacle.valRadius = valL(valRadius)
                                    obstacle.uomRadius = valL(uomRadius)
                                    obstacle.datetimeValidWef = valL(datetimeValidWef)
                                    obstacle.datetimeValidTil = valL(datetimeValidTil)
                                    obstacle.txtRmk = valL(txtRmk)
                                    obstacle.txtName = valL(name)

                                    ' handle coord format

                                    Dim latFac As Long = 1
                                    Dim lonFac As Long = 1
                                    If valL(latitude).Contains("N") Or valL(latitude).Contains("S") And valL(longitutde).Contains("E") Or valL(longitutde).Contains("W") Then

                                        If valL(latitude).Contains("S") Then
                                            latFac = -1
                                        End If

                                        If valL(longitutde).Contains("W") Then
                                            lonFac = -1
                                        End If

                                        obstacle.geoLat = valL(latitude).Replace(".", ReplaceTo).Replace("N", "").Replace("S", "") * latFac
                                        obstacle.geoLong = valL(longitutde).Replace(".", ReplaceTo).Replace("E", "").Replace("W", "") * lonFac
                                    Else
                                        obstacle.geoLat = valL(latitude).Replace(".", ReplaceTo)
                                        obstacle.geoLong = valL(longitutde).Replace(".", ReplaceTo)
                                    End If


                                    ' add to buffer array, for later tile export
                                    Dim xIdx As Long = Math.Floor(obstacle.geoLong) + 180
                                    Dim yIdx As Long = Math.Floor(obstacle.geoLat) + 90

                                    If TileX(xIdx) Is Nothing Then TileX(xIdx) = New List(Of String)
                                    If TileY(yIdx) Is Nothing Then TileY(yIdx) = New List(Of String)

                                    If id = -1 Then
                                        Dim tempId = valL(obsId) + idCntr
                                        If TileX(xIdx).Contains(tempId) = False Then TileX(xIdx).Add(tempId)
                                        If TileY(yIdx).Contains(tempId) = False Then TileY(yIdx).Add(tempId)
                                    Else
                                        If TileX(xIdx).Contains(id) = False Then TileX(xIdx).Add(id)
                                        If TileY(yIdx).Contains(id) = False Then TileY(yIdx).Add(id)
                                    End If

                                    ' min / max estimation
                                    If minX = -200 Or obstacle.geoLong < minX Then minX = obstacle.geoLong
                                    If maxX = -200 Or obstacle.geoLong > maxX Then maxX = obstacle.geoLong

                                    If minY = -200 Or obstacle.geoLat < minY Then minY = obstacle.geoLat
                                    If maxY = -200 Or obstacle.geoLat > maxY Then maxY = obstacle.geoLat

                                    'Try
                                    '    If defaultHeightFlag > -1 Then obstacle.DefaultHeight = valL(defaultHeightFlag)
                                    'Catch ex As Exception

                                    'End Try


                                    Select Case obstacle.uomDistVer
                                        Case "M"
                                            If obstacle.valHgt > 300 Then
                                                Console.WriteLine("WARN: Height over 300 M, set to 300 M default height")
                                                obstacle.codeHgtAccuracy = True
                                                obstacle.valHgt = 300
                                            End If

                                        Case "FT"
                                            If obstacle.valHgt > 900 Then
                                                Console.WriteLine("WARN: Height over 900 FT, set to 300 M default height")
                                                obstacle.codeHgtAccuracy = True
                                                obstacle.valHgt = 900
                                            End If
                                    End Select




                                    obstacle.ObsUidLink = linkId - 1
                                    obstacle.codeLinkType = valL(linktype)

                                    If obstacle.codeLinkType = "" Or obstacle.codeLinkType = "NULL" Then
                                        obstacle.codeLinkType = "GROUP"
                                    End If

                                    obstacle.codeType = valL(codeType)

                                    'Select Case obstacle.txtDescrType
                                    '    Case "POWERLINE", "CABLEWAY"
                                    '        obstacle.txtDescrType = "MAST"
                                    '    Case "BUILDING", "TOWER"
                                    '        obstacle.txtDescrType = "TOWER"
                                    '    Case "CRANE"
                                    '        obstacle.txtDescrType = "CRANE"
                                    '    Case Else
                                    '        obstacle.txtDescrType = "TOWER"
                                    'End Select

                                    obstacleLst.Add(obstacle)
                                    linkId += 1

                                    ' remove item to not recognize again
                                    lines(l) = ""
                                Else
                                    linkId = 1
                                    End If

                                    If id = -1 Then Exit For

                                End If
                            Catch ex As Exception
                                handleException(ex, Reflection.MethodBase.GetCurrentMethod, "cant read obstalce csv entry")
                            End Try
                        Next



                        Dim f As New DataFormatStruct
                        f.Attributes = obstacleGroup
                        f.AttributesPointer1 = obstacleLst.ToArray
                        If id = -1 Then
                            f.RootAttribute = valL(obsId) + idCntr
                        Else
                            f.RootAttribute = id

                        End If

                        f.Datatype = "Ogr"
                        midCntr += 1
                        retLst.Add(f)
                        Console.WriteLine("INFO: added OGR " & f.Attributes.txtName & "with " & obstacleLst.Count & " elements")
                    End If
            Catch ex As Exception
                handleException(ex, Reflection.MethodBase.GetCurrentMethod, "cant read obstalce csv entry")
            End Try
        Next

        Console.WriteLine("data boundingbox is: longitude: " & minX & " to " & maxX & ", latitude: " & minY & " to " & maxY)



        For finalDbCntr As Long = 0 To retLst.Count - 1
            For compareDbCntr As Long = 0 To retLst.Count - 1

                If Not compareDbCntr = finalDbCntr Then
                    If retLst(finalDbCntr).AttributesPointer1 IsNot Nothing Then
                        For Each cnn In retLst(finalDbCntr).AttributesPointer1
                            If cnn IsNot Nothing And retLst(compareDbCntr).AttributesPointer1 IsNot Nothing Then

                                If cnn.codeLinkType = "cable" Then
                                    GoTo FFF
                                End If

                                'Console.WriteLine(cnn.codeLinkType)
                                Select Case cnn.codeLinkType.ToString.ToUpper

                                    Case "GROUP"

                                        ' second comparer loop
                                        For Each compCnn In retLst(compareDbCntr).AttributesPointer1
                                            Select Case compCnn.codeLinkType.ToString.ToUpper
                                                Case "GROUP"

                                                    Dim p1 = New Drawing.PointF(cnn.geolong, cnn.geoLat)
                                                    Dim p2 = New Drawing.PointF(compCnn.geoLong, compCnn.geoLat)

                                                    Dim dist = GetGreatCircleDistance_ConstEarthRadiusInNm(p1.X, p2.X, p1.Y, p2.Y)

                                                    If dist = 0 Then
                                                        Console.WriteLine("ERR:doublicate " & retLst(compareDbCntr).RootAttribute)

                                                        retLst(compareDbCntr) = Nothing

                                                    Else
                                                        If dist < 1 Then
                                                            Dim kr = "Juhu"

                                                            If retLst(finalDbCntr).AttributesPointer1 IsNot Nothing Then


                                                                Dim qqq = retLst(finalDbCntr)
                                                                Dim newLength = retLst(finalDbCntr).AttributesPointer1.length
                                                                ReDim Preserve qqq.AttributesPointer1(newLength)
                                                                qqq.AttributesPointer1(newLength) = compCnn


                                                                retLst(finalDbCntr) = qqq

                                                                Console.WriteLine("embedded " & compareDbCntr & " into " & finalDbCntr)

                                                                ' delete the old one
                                                                retLst(compareDbCntr) = Nothing
                                                            End If
                                                        End If

                                                    End If



                                            End Select
                                        Next
                                End Select

                            End If
                        Next
                    End If

                End If

            Next
FFF:
        Next


        ' do regrouping


        Dim lowerX As Short = Math.Floor(minX)
        Dim upperX As Short = Math.Ceiling(maxX)

        Dim lowerY As Short = Math.Floor(minY)
        Dim upperY As Short = Math.Ceiling(maxY)


        Dim settings As New XmlWriterSettings()
        settings.Encoding = System.Text.Encoding.UTF8
        settings.Indent = True


        If Not System.IO.Directory.Exists(System.Environment.CurrentDirectory & "\out") Then System.IO.Directory.CreateDirectory(System.Environment.CurrentDirectory & "\out")

        For x As Long = lowerX + 180 To upperX + 180
            For y As Long = lowerY + 90 To upperY + 90


                If TileY(y) IsNot Nothing And TileX(x) IsNot Nothing Then
                    Dim XmlWrt As XmlWriter = XmlWriter.Create("out/" & System.IO.Path.GetFileName(file) & "_" & x - 180 & "_" & y - 90 & ".ofmx", settings)
                    With XmlWrt
                        ' Write the Xml declaration.
                        .WriteStartDocument()

                        'Write the root element.
                        .WriteStartElement("OFMX-Snapshot")

                        .WriteAttributeString("version", "", "1.0")
                        XmlWrt.WriteAttributeString("xmlns", "xsi", Nothing, "http://www.w3.org/2001/XMLSchema-instance")

                        XmlWrt.WriteAttributeString("xsi", "noNamespaceSchemaLocation", Nothing, "https://openflightmaps.org/schema/0/OFMX-Snapshot.xsd")

                        .WriteAttributeString("effective", "", String.Format(Date.UtcNow, "yyyy-MM-dd") & "T" & String.Format(Date.UtcNow, "HH:mm:ss"))
                        .WriteAttributeString("origin", "", "csvObstacle2ofmx")
                        .WriteAttributeString("created", "", String.Format(Date.UtcNow, "yyyy-MM-dd") & "T" & String.Format(Date.UtcNow, "HH:mm:ss"))
                        .WriteAttributeString("namespace", "", "210444d1-4576-e92d-0983-4669182a8c04")
                        .WriteAttributeString("boundingbox", "", x - 180 & ";" & y - 90 & ";1;1;")



                        Dim cn As Long = 0
                        For Each ogr In retLst

                            If TileX(x).Contains(ogr.RootAttribute) And TileY(y).Contains(ogr.RootAttribute) Then
                                writeObstacle(XmlWrt, ogr, idCntr)
                                cn += 1
                                idCntr += 1
                            End If

                        Next



                        .WriteEndElement() ' this closes the Root Element
                        .Close()

                        If cn = 0 Then
                            System.IO.File.Delete("out/" & System.IO.Path.GetFileName(file) & "_" & x - 180 & "_" & y - 90 & ".ofmx")
                        Else

                            Console.WriteLine("INFO: wrote " & cn & " Elements for area: lon " & x - 180 & " lat " & y - 90)
                        End If
                    End With
                End If

            Next
        Next



        Return retLst.ToArray



    End Function

    Function getIdOfHeader(str As String, arr() As String) As Short

        Dim i As Long = 0
        For Each item In arr
            item = item.Trim(" ")
            If str = item Then Return i
            i += 1
        Next
        Return -1
    End Function
    Sub handleException(ex As Exception, functionName As MethodBase, Optional key As String = "", Optional noConsole As Boolean = False)

        If Not noConsole Then Console.WriteLine("ERR: " & functionName.Name & " - " & key)
    End Sub
    Sub writeObstacle(xmlWriter As XmlWriter, x As DataFormatStruct, rootAttribute As String)


        Dim ogrUid As String = getUUID(x.Attributes.txtName & x.Attributes.origin)



        ' set client region if has not been set
        xmlWriter.WriteStartElement("Ogr")

        xmlWriter.WriteStartElement("OgrUid")

        'xmlWriter.WriteAttributeString("dbUid", rootAttribute)
        'xmlWriter.WriteAttributeString("mid", ogrUid)

        ' required to identify entity as new
        If x.isNewEntity Then xmlWriter.WriteAttributeString("newEntity", True)
        If x.importMode <> "" Then xmlWriter.WriteAttributeString("m", x.importMode)
        xmlWriter.WriteElementString("txtName", x.Attributes.txtName)
        xmlWriter.WriteElementString("geoLong", toOfmxLon(x.AttributesPointer1(0).geoLong))
        xmlWriter.WriteElementString("geoLat", toOfmxLat(x.AttributesPointer1(0).geoLat))

        xmlWriter.WriteEndElement() ' ObsUid
        xmlWriter.WriteElementString("codeDatum", "WGE")
        xmlWriter.WriteElementString("valGeoAccuracy", x.Attributes.valGeoAccuracy)
        xmlWriter.WriteElementString("uomGeoAccuracy", x.Attributes.uomGeoAccuracy)
        xmlWriter.WriteElementString("valElevAccuracy", x.Attributes.valElevAccuracy)
        xmlWriter.WriteElementString("uomElevAccuracy", x.Attributes.uomElevAccuracy)
        xmlWriter.WriteElementString("txtRmk", x.Attributes.txtRmk)

        xmlWriter.WriteEndElement() ' ogr

        ' write all elements
        Dim cntr As Short = 0
        If x.AttributesPointer1 IsNot Nothing Then
            For Each el In x.AttributesPointer1

                Dim obsUid As String = getUUID(el.geolat & el.geoLong & x.Attributes.txtName & x.Attributes.origin)

                xmlWriter.WriteStartElement("Obs")

                xmlWriter.WriteStartElement("ObsUid")
                'xmlWriter.WriteAttributeString("dbUid", rootAttribute & "." & cntr)
                'xmlWriter.WriteAttributeString("mid", ogrUid)

                xmlWriter.WriteStartElement("OgrUid")
                'xmlWriter.WriteAttributeString("dbUid", rootAttribute)
                xmlWriter.WriteElementString("txtName", x.Attributes.txtName)
                xmlWriter.WriteElementString("geoLong", toOfmxLon(x.AttributesPointer1(0).geoLong))
                xmlWriter.WriteElementString("geoLat", toOfmxLat(x.AttributesPointer1(0).geoLat))

                xmlWriter.WriteEndElement() ' OrgUid

                Dim PosLat As String = toOfmxLat(el.geoLat)
                Dim PosLon As String = toOfmxLon(el.geoLong)

                xmlWriter.WriteElementString("geoLat", PosLat)
                xmlWriter.WriteElementString("geoLong", PosLon)

                xmlWriter.WriteEndElement() ' obsUid

                xmlWriter.WriteElementString("txtName", el.txtName)
                xmlWriter.WriteElementString("codeDatum", "WGE")
                xmlWriter.WriteElementString("codeType", el.codeType)

                If el.codeLgt IsNot Nothing Then
                    If el.codeLgt Then
                        xmlWriter.WriteElementString("codeLgt", "Y")
                    Else
                        xmlWriter.WriteElementString("codeLgt", "N")
                    End If
                Else
                    xmlWriter.WriteElementString("codeLgt", "N")
                End If

                If el.codeMarking IsNot Nothing Then
                    If el.codeMarking Then
                        xmlWriter.WriteElementString("codeMarking", "Y")
                    Else
                        xmlWriter.WriteElementString("codeMarking", "N")
                    End If
                Else
                    xmlWriter.WriteElementString("codeMarking", "")
                End If

                If el.txtDescrLgt IsNot Nothing Then
                    xmlWriter.WriteElementString("txtDescrLgt", el.txtDescrLgt)
                Else
                    xmlWriter.WriteElementString("txtDescrLgt", "")
                End If

                If el.txtDescrMarking IsNot Nothing Then
                    xmlWriter.WriteElementString("txtDescrMarking", el.txtDescrMarking)
                Else
                    xmlWriter.WriteElementString("txtDescrMarking", "")
                End If

                xmlWriter.WriteElementString("codeDatum", "WGE")

                xmlWriter.WriteElementString("valElev", el.valElev)
                xmlWriter.WriteElementString("valHgt", el.valHgt)
                xmlWriter.WriteElementString("uomDistVer", el.uomDistVer)

                If el.codeHgtAccuracy IsNot Nothing Then
                    If el.codeHgtAccuracy Then
                        xmlWriter.WriteElementString("codeHgtAccuracy", "Y")
                    Else
                        xmlWriter.WriteElementString("codeHgtAccuracy", "N")
                    End If
                Else
                    xmlWriter.WriteElementString("codeHgtAccuracy", "N")
                End If

                If el.valRadius IsNot Nothing Then
                    xmlWriter.WriteElementString("valRadius", el.valRadius)
                Else
                    xmlWriter.WriteElementString("valRadius", "")
                End If

                If el.uomRadius IsNot Nothing Then
                    xmlWriter.WriteElementString("uomRadius", el.uomRadius)
                Else
                    xmlWriter.WriteElementString("uomRadius", "")
                End If


                ' extensions
                If x.AttributesPointer1.Length > 1 Then
                    xmlWriter.WriteElementString("codeLinkType", el.codeLinkType)
                    xmlWriter.WriteElementString("codeGroup", "Y")

                    xmlWriter.WriteStartElement("ObsUidLink")
                    'xmlWriter.WriteAttributeString("dbUid", rootAttribute & "." & cntr)
                    'xmlWriter.WriteAttributeString("mid", ogrUid)

                    xmlWriter.WriteStartElement("OgrUid")
                    'xmlWriter.WriteAttributeString("dbUid", rootAttribute)
                    xmlWriter.WriteElementString("txtName", x.Attributes.txtName)
                    xmlWriter.WriteElementString("geoLong", toOfmxLon(x.AttributesPointer1(0).geoLong))
                    xmlWriter.WriteElementString("geoLat", toOfmxLat(x.AttributesPointer1(0).geoLat))

                    xmlWriter.WriteEndElement() ' OrgUid

                    Dim LinkLon As String = toOfmxLon(x.AttributesPointer1(el.ObsUidLink).geoLong)
                    Dim LinkLat As String = toOfmxLat(x.AttributesPointer1(el.ObsUidLink).geoLat)

                    xmlWriter.WriteElementString("geoLat", LinkLat)
                    xmlWriter.WriteElementString("geoLong", LinkLon)

                    xmlWriter.WriteEndElement() ' ObsUidLink

                Else
                    xmlWriter.WriteElementString("codeGroup", "N")

                End If


                xmlWriter.WriteElementString("datetimeValidWef", el.datetimeValidWef)
                xmlWriter.WriteElementString("datetimeValidTil", el.datetimeValidTil)

                If el.txtRmk IsNot Nothing Then
                    xmlWriter.WriteElementString("txtRmk", el.txtRmk)
                Else
                    xmlWriter.WriteElementString("txtRmk", "")
                End If

                xmlWriter.WriteEndElement() ' obs'
                cntr += 1
            Next

        End If

        ' ...



    End Sub

    Function toOfmxLon(lon As Double) As String
        Dim SuffLon As String = "W"
        If lon > 0 Then SuffLon = "E"

        Return String.Format(lon, "0.00000000").ToString.Replace(",", ".") & SuffLon
    End Function

    Function toOfmxLat(lat As Double) As String
        Dim SuffLat As String = "S"
        If lat > 0 Then SuffLat = "N"

        Return String.Format(lat, "0.00000000").ToString.Replace(",", ".") & SuffLat
    End Function


    Structure VectorStruct
        Dim x As Double
        Dim y As Double
        Dim z As Double
    End Structure
    ' Function TileNbr to Position
    <Serializable()> Structure DoublePointStruct
        Dim x As Double
        Dim y As Double
        Dim rmk As String
    End Structure

    ' Coordinate Transformations
    Dim EarthRadius As Double = 6378.137 / 1.852 ' in nautical Miles

    Function GetGreatCircleDistance_ConstEarthRadiusInNm(x1 As Double, x2 As Double, y1 As Double, y2 As Double) As Double

        Dim position1 As DoublePointStruct
        Dim position2 As DoublePointStruct

        If x1 = x2 And y1 = y2 Then Return 0

        position1.x = x1
        position2.x = x2
        position1.y = y1
        position2.y = y2
        ' Notes from 22.3.2012
        Dim V1 As VectorStruct
        V1.x = EarthRadius * (Math.Cos(position1.y * Math.PI / 180) * Math.Cos(position1.x * Math.PI / 180))
        V1.y = EarthRadius * (Math.Cos(position1.y * Math.PI / 180) * Math.Sin(position1.x * Math.PI / 180))
        V1.z = EarthRadius * (Math.Sin(position1.y * Math.PI / 180))

        Dim V2 As VectorStruct
        V2.x = EarthRadius * (Math.Cos(position2.y * Math.PI / 180) * Math.Cos(position2.x * Math.PI / 180))
        V2.y = EarthRadius * (Math.Cos(position2.y * Math.PI / 180) * Math.Sin(position2.x * Math.PI / 180))
        V2.z = EarthRadius * (Math.Sin(position2.y * Math.PI / 180))

        Dim AngleBetw As Double = Math.Acos((V1.x * V2.x + V1.y * V2.y + V1.z * V2.z) / (Math.Sqrt(V1.x ^ 2 + V1.y ^ 2 + V1.z ^ 2) * Math.Sqrt(V2.x ^ 2 + V2.y ^ 2 + V2.z ^ 2)))

        ' Return Distance in nautical Miles
        Return AngleBetw * EarthRadius
    End Function

    Public Function getUUID(uidStr As String) As String
        'debug
        ' Return uidStr

        Try


            'If uidStr.Contains("LSGG 6") Then
            '    Dim ollid = "AseUid|LSAS|TMA|LSGG 6"
            'End If
            If uidStr Is Nothing Then Return ""
            Using hasher As MD5 = MD5.Create()    ' create hash object

                ' Convert to byte array and get hash
                Dim dbytes As Byte() =
                 hasher.ComputeHash(Encoding.UTF8.GetBytes(uidStr))

                ' sb to create string from bytes
                Dim sBuilder As New StringBuilder()

                ' convert byte data to hex string
                For n As Integer = 0 To dbytes.Length - 1
                    sBuilder.Append(dbytes(n).ToString("X2"))
                Next n

                Dim rawhash As String = sBuilder.ToString().ToLower

                Dim finalUuid As String = ""
                Dim cnt As Short = 0
                For Each letter In rawhash

                    If cnt = 8 Or cnt = 12 Or cnt = 16 Or cnt = 20 Then
                        finalUuid &= "-"
                    End If
                    finalUuid &= letter
                    cnt += 1
                Next

                Return finalUuid
            End Using
        Catch ex As Exception

        End Try
        Return 0
    End Function

End Module
