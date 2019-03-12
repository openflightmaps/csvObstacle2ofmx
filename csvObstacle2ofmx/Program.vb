Imports System
Imports System.Reflection
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
        Dim txtDescrType As String
        Dim lateralPrecision As Double
        Dim verticalPrecision As Double
        Dim obstacleRadius As Double
        Dim origin As String
    End Structure

    Structure obstacleStruct
        Dim txtDescrType As String
        Dim xt_linkType As String
        Dim xt_linkedToGroupInternalId As Short
        Dim codeLgt As String
        Dim txtDescrLgt As String
        Dim uomDistVer As String
        Dim valHgt As Double
        Dim valElev As Double
        Dim geoLat As Double
        Dim geoLong As Double
        Dim xt_defaultHeightFlag As String
        Dim xt_lockForImport As Boolean
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
            If args(i) = "-f" Then
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


        Dim headLine() As String = lines(0).Split(";")
        Dim groupId = getIdOfHeader("codeGroupId", headLine)
        Dim name = getIdOfHeader("txtName", headLine)
        Dim groupInternalId = getIdOfHeader("locGroupMemberId", headLine)
        Dim linkedToGroupInternalId = getIdOfHeader("locLinkedToGroupMemberId", headLine)
        Dim linktype = getIdOfHeader("codeLinkType", headLine)
        Dim type = getIdOfHeader("codeType", headLine)
        Dim lighted = getIdOfHeader("codeLgt", headLine)
        Dim markingDescription = getIdOfHeader("txtDescrMarking", headLine)
        Dim heightUnit = getIdOfHeader("uomDistVer", headLine)
        Dim heightValue = getIdOfHeader("valHgt", headLine)
        Dim ElevationValue = getIdOfHeader("valElev", headLine)
        Dim latitude As Double = getIdOfHeader("geoLat", headLine)
        Dim longitutde As Double = getIdOfHeader("geoLong", headLine)
        Dim defaultHeightFlag = getIdOfHeader("defaultHeightFlag", headLine)
        Dim lateralPrecision = getIdOfHeader("valRadiusAccuracy", headLine)
        Dim verticalPrecision = getIdOfHeader("codeHgtAccuracy", headLine)
        Dim obstacleRadius = getIdOfHeader("valRadius", headLine)
        Dim source = getIdOfHeader("source", headLine)
        Dim retLst As New List(Of DataFormatStruct)


        Dim midCntr As Short = 0

        For i As Long = 1 To lines.Length - 1

            Dim obstacleGroup As New obstacleGroupStruct
            lines(i) = lines(i).Replace(vbNewLine, " ")
            Dim val = lines(i).Split(";")
            Try

                If val.Length > 10 Then
                    If val(lateralPrecision) <> "" Then obstacleGroup.lateralPrecision = val(lateralPrecision)
                    If val(obstacleRadius) <> "" Then obstacleGroup.obstacleRadius = val(obstacleRadius)
                    obstacleGroup.txtName = val(name)
                    obstacleGroup.origin = val(source)

                    Dim id = val(groupId)

                    ' find all childs
                    Dim obstacleLst As New List(Of obstacleStruct)
                    Dim linkId As Short = 1
                    For l As Long = 1 To lines.Length - 1
                        Dim valL = lines(l).Split(";")
                        Try


                            If valL.Length > 1 Then


                                If id = valL(groupId) Then

                                    Dim obstacle As New obstacleStruct


                                    obstacle.codeLgt = valL(lighted)

                                    obstacle.geoLat = valL(latitude).Replace(".", ReplaceTo)
                                    obstacle.geoLong = valL(longitutde).Replace(".", ReplaceTo)

                                    ' add to buffer array, for later tile export
                                    Dim xIdx As Long = Math.Floor(obstacle.geoLong) + 180
                                    Dim yIdx As Long = Math.Floor(obstacle.geoLat) + 90

                                    If TileX(xIdx) Is Nothing Then TileX(xIdx) = New List(Of String)
                                    If TileY(yIdx) Is Nothing Then TileY(yIdx) = New List(Of String)

                                    If TileX(xIdx).Contains(id) = False Then TileX(xIdx).Add(id)
                                    If TileY(yIdx).Contains(id) = False Then TileY(yIdx).Add(id)

                                    ' min / max estimation
                                    If minX = -200 Or obstacle.geoLong < minX Then minX = obstacle.geoLong
                                    If maxX = -200 Or obstacle.geoLong > maxX Then maxX = obstacle.geoLong

                                    If minY = -200 Or obstacle.geoLat < minY Then minY = obstacle.geoLat
                                    If maxY = -200 Or obstacle.geoLat > maxY Then maxY = obstacle.geoLat

                                    obstacle.txtDescrLgt = valL(markingDescription)
                                    obstacle.uomDistVer = valL(heightUnit)
                                    obstacle.valElev = valL(ElevationValue)
                                    obstacle.valHgt = valL(heightValue)
                                    obstacle.xt_defaultHeightFlag = valL(defaultHeightFlag)
                                    obstacle.xt_linkedToGroupInternalId = linkId - 1
                                    obstacle.xt_linkType = valL(linktype)

                                    If obstacle.xt_linkType = "" Or obstacle.xt_linkType = "NULL" Then
                                        obstacle.xt_linkType = "GROUP"
                                    End If

                                    obstacle.xt_lockForImport = False
                                    obstacle.txtDescrType = valL(type)

                                    Select Case obstacle.txtDescrType
                                        Case "POWERLINE", "CABLEWAY"
                                            obstacle.txtDescrType = "MAST"
                                        Case "BUILDING", "TOWER"
                                            obstacle.txtDescrType = "TOWER"
                                        Case "CRANE"
                                            obstacle.txtDescrType = "CRANE"
                                        Case Else
                                            obstacle.txtDescrType = "TOWER"
                                    End Select

                                    obstacleLst.Add(obstacle)
                                    linkId += 1

                                    ' remove item to not recognize again
                                    lines(l) = ""
                                Else
                                    linkId = 1
                                End If
                            End If
                        Catch ex As Exception
                            handleException(ex, Reflection.MethodBase.GetCurrentMethod, "cant read obstalce csv entry")
                        End Try
                    Next

                    Dim f As New DataFormatStruct
                    f.Attributes = obstacleGroup
                    f.AttributesPointer1 = obstacleLst.ToArray


                    f.RootAttribute = midCntr
                    f.Datatype = "Ogr"
                    midCntr += 1
                    retLst.Add(f)
                    Console.WriteLine("INFO: added OGR " & f.Attributes.txtName)
                End If
            Catch ex As Exception
                handleException(ex, Reflection.MethodBase.GetCurrentMethod, "cant read obstalce csv entry")
            End Try
        Next

        Console.WriteLine("data boundingbox is: longitude: " & minX & " to " & maxX & ", latitude: " & minY & " to " & maxY)

        Dim lowerX As Short = Math.Floor(minX)
        Dim upperX As Short = Math.Ceiling(maxX)

        Dim lowerY As Short = Math.Floor(minY)
        Dim upperY As Short = Math.Ceiling(maxY)


        Dim settings As New XmlWriterSettings()
        settings.Encoding = System.Text.Encoding.UTF8
        settings.Indent = True


        If Not System.IO.Directory.Exists(System.Environment.CurrentDirectory & "\out") Then System.IO.Directory.CreateDirectory(System.Environment.CurrentDirectory & "\out")
        Dim idCntr As Long = 5017342

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

        ' set client region if has not been set
        xmlWriter.WriteStartElement("Ogr")

        xmlWriter.WriteStartElement("OgrUid")

        xmlWriter.WriteAttributeString("dbUid", rootAttribute)

        ' required to identify entity as new
        If x.isNewEntity Then xmlWriter.WriteAttributeString("newEntity", True)
        If x.importMode <> "" Then xmlWriter.WriteAttributeString("m", x.importMode)
        xmlWriter.WriteElementString("txtName", x.Attributes.txtName)


        xmlWriter.WriteEndElement() ' ObsUid


        xmlWriter.WriteElementString("lateralPrecision", x.Attributes.lateralPrecision)
        xmlWriter.WriteElementString("verticalPrecision", x.Attributes.verticalPrecision)
        xmlWriter.WriteElementString("origin", x.Attributes.origin)
        xmlWriter.WriteEndElement() ' ogr

        ' write all elements
        Dim cntr As Short = 0
        If x.AttributesPointer1 IsNot Nothing Then
            For Each el In x.AttributesPointer1
                xmlWriter.WriteStartElement("Obs")

                xmlWriter.WriteStartElement("ObsUid")
                xmlWriter.WriteAttributeString("dbUid", rootAttribute & "." & cntr)

                xmlWriter.WriteStartElement("OgrUid")
                xmlWriter.WriteAttributeString("dbUid", rootAttribute)
                xmlWriter.WriteElementString("txtName", x.Attributes.txtName)
                xmlWriter.WriteEndElement() ' OrgUid

                Dim SuffLat As String = "S"
                If el.geoLat > 0 Then SuffLat = "N"
                Dim SuffLon As String = "W"
                If el.geoLong > 0 Then SuffLon = "E"

                Dim PosLat As String = String.Format(el.geoLat, "0.00000000").ToString.Replace(",", ".") & SuffLat
                Dim PosLon As String = String.Format(el.geoLong, "0.00000000").ToString.Replace(",", ".") & SuffLon


                xmlWriter.WriteElementString("geoLat", PosLat)
                xmlWriter.WriteElementString("geoLong", PosLon)

                xmlWriter.WriteEndElement() ' obsUid


                xmlWriter.WriteElementString("txtDescrType", el.txtDescrType)
                ' xmlWriter.WriteElementString("codeGroup", el.codeGroup)
                xmlWriter.WriteElementString("codeLgt", el.codeLgt)
                xmlWriter.WriteElementString("txtDescrLgt", el.txtDescrLgt)
                xmlWriter.WriteElementString("valElev", el.valElev)
                xmlWriter.WriteElementString("valHgt", el.valHgt)
                xmlWriter.WriteElementString("uomDistVer", el.uomDistVer)

                ' extensions
                xmlWriter.WriteElementString("xt_defaultHeightFlag", el.xt_defaultHeightFlag)
                xmlWriter.WriteElementString("xt_linkedToGroupInternalId", el.xt_linkedToGroupInternalId)
                xmlWriter.WriteElementString("xt_linkType", el.xt_linkType)
                xmlWriter.WriteElementString("xt_lockForImport", el.xt_lockForImport)


                xmlWriter.WriteEndElement() ' obs'
                cntr += 1
            Next

        End If

        ' ...



    End Sub
End Module
