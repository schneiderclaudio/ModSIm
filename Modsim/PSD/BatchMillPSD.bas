Attribute VB_Name = "MdlBatchMill"
Option Explicit
Private K(25) As Single, B(25, 25) As Single
Private c(25, 25) As Single
Private Revs As Single
Private NoLoadPower As Single, MillPower As Single

Public Sub SimulateBatchMill()
'****************************
Dim I As Integer, J As Integer
Dim FEED As SIZEDATA
Dim PM As Single, P As Single, PSIZE As Single
Dim T As Single

' Interpolate the size distribution
PM = 1!
'Set up the feed
FEED.NDC = 20
FEED.size(1) = CURRDATA(FeedIndex).CUMSIZE(1)
For I = 2 To FEED.NDC
  FEED.size(I) = FEED.size(I - 1) / Sqr(2#)
Next I
   
For J = 1 To FEED.NDC - 1
  PSIZE = Sqr(FEED.size(J) * FEED.size(J + 1))
  Call FRPASS(CURRDATA(FeedIndex).CUMSIZE(), CURRDATA(FeedIndex).fraction(), CURRDATA(FeedIndex).NDC, P, PSIZE)
  FEED.fraction(J) = PM - P
  FEED.CUMSIZE(J) = PSIZE
  PM = P
Next J
FEED.fraction(FEED.NDC) = PM
Call BatchMillCoeffs(FEED.NDC, FEED.size, FEED.CUMSIZE)
T = CURRMODELDATA(ID).PARAM(8)
Call BatchMill(FEED, BMProduct(1), T)
'CurrentIndex = Val(Sizgraph.StreamList.List(Sizgraph.StreamList.ListCount)) + 1
CurrentIndex = NumberOfDataSets + 1
Sizgraph.StreamList.AddItem CStr(CurrentIndex) & " Ball mill product"
BMProduct(1).NDC = FEED.NDC
BMProduct(1).ID = CurrentIndex
BMProduct(1).SET_NAME = "Product after " & Format(T, "##0") & " minutes"
CURRDATA(CurrentIndex) = BMProduct(1)
'Convert to cumulative distribution
CURRDATA(CurrentIndex).fraction(1) = 1# - BMProduct(1).fraction(1)
For I = 2 To FEED.NDC
  CURRDATA(CurrentIndex).fraction(I) = CURRDATA(CurrentIndex).fraction(I - 1) - BMProduct(1).fraction(I)
Next I
NumberOfDataSets = NumberOfDataSets + 1
Sizgraph.Show
End Sub

Public Sub BatchMill(FEED As SIZEDATA, Sample As SIZEDATA, SampleTime As Single)
'*******************************************************************************
Dim T As Single
Dim Alpha(25, 25) As Single, A(25) As Single
Dim I As Integer, J As Integer
On Error GoTo ErrHandler
  For I = 1 To FEED.NDC
    Sample.size(I) = FEED.size(I)
    Sample.CUMSIZE(I) = FEED.CUMSIZE(I)
    A(I) = FEED.fraction(I)
    For J = 1 To I - 1
      A(I) = A(I) - c(I, J) * A(J)
    Next J
  Next I
  T = SampleTime
  For I = 1 To FEED.NDC
    Sample.fraction(I) = 0
    For J = 1 To I
      Sample.fraction(I) = Sample.fraction(I) + c(I, J) * A(J) * Exp(-K(J) * T)
    Next J
  Next I
  Exit Sub
ErrHandler:
  Exit Sub
End Sub

Public Sub BatchMillCoeffs(NDC As Integer, size() As Single, CUMSIZE() As Single)
'********************************************************************************
'Calculate the size distribution at time
Dim k0 As Single, Alpha As Single, mu As Single, Lambda As Single
Dim N1 As Single, N2 As Single, PHI As Single
Dim I As Integer, J As Integer
On Error GoTo ErrHandler

  k0 = CURRMODELDATA(ID).PARAM(1)
  Alpha = CURRMODELDATA(ID).PARAM(2)
  mu = CURRMODELDATA(ID).PARAM(3)       'In mm
  Lambda = CURRMODELDATA(ID).PARAM(4)
  N1 = CURRMODELDATA(ID).PARAM(5)
  N2 = CURRMODELDATA(ID).PARAM(6)
  PHI = CURRMODELDATA(ID).PARAM(7)
  
  For I = 1 To NDC
    K(I) = AustinSelection(k0, Alpha, mu, Lambda, size(I))
    For J = 1 To I - 1
      B(I, J) = AustinBrFunction(PHI, N1, N2, CUMSIZE(I - 1), CUMSIZE(J)) - AustinBrFunction(PHI, N1, N2, CUMSIZE(I), CUMSIZE(J))
    Next J
  Next I
  K(NDC) = 0

  Call BallMillMatrix(NDC)
  Exit Sub
ErrHandler:
  MsgBox "Calculation error", vbCritical, "ERROR"
  Exit Sub
End Sub
Public Function AustinBrFunction(PHI As Single, N1 As Single, N2 As Single, X As Single, Y As Single) As Single
'************************************************************************************************************
AustinBrFunction = PHI * (X / Y) ^ N1 + (1 - PHI) * (X / Y) ^ N2
End Function

Public Function AustinSelection(k0 As Single, Alpha As Single, mu As Single, Lambda As Single, size As Single) As Single
'***********************************************************************************************************************
Dim dp As Single
dp = size * 1000
AustinSelection = k0 * dp ^ Alpha / (1 + (dp / mu) ^ Lambda)
End Function

Public Sub BallMillMatrix(NDC As Integer)
'****************************************
'Calculates the size distribution in a batch mill
Dim I As Integer, J As Integer, L As Integer
On Error GoTo ErrHandler
 
  'Calculate the coefficients
  For I = 1 To NDC
    c(I, I) = 1#
    For L = 1 To I - 1
      c(I, L) = 0#
      For J = L To I - 1
        c(I, L) = B(I, J) * K(J) * c(J, L) + c(I, L)
      Next J
      c(I, L) = c(I, L) / (K(I) - K(L))
    Next L
  Next I
  Exit Sub

ErrHandler:
  Exit Sub
End Sub


Sub FRPASS(CUMSIZ() As Single, Cum() As Single, NDC As Integer, P As Single, PSIZE As Single)
'********************************************************************************************
'  CALCULATES THE FRACTION PASSING A GIVEN SIZE. DISTRIBUTION IS ASSUMED
'  TO BE LOCALLY ROSIN-RAMMLER.

Dim COND1 As Integer, COND2 As Integer
Dim ND As Integer, I As Integer
Dim XI As Single, ETA As Single
Dim Alpha As Single
  If PSIZE <= 0# Then
    'MsgBox "The system is attempting to calculate the fraction passing size zero." & Chr(13) & Chr(10) & "This indicates an inconsistency in your data", 48, "WARNING"
    P = 0#
    Exit Sub
  End If
  ND = NDC - 2
  I = 1
  P = 1!
  If (CUMSIZ(1) > PSIZE) Then
    For I = 1 To ND
      If (CUMSIZ(I) <= PSIZE And Cum(I) >= 1!) Then Exit Sub
      COND1 = CUMSIZ(I) > PSIZE And CUMSIZ(I + 1) <= PSIZE And Cum(I) < 1!
      COND2 = CUMSIZ(I) <= PSIZE And Cum(I) < 1!
      If COND1 Or COND2 Then Exit For
    Next I
  End If
  If I = ND + 1 Then
    If Cum(NDC - 1) > 0# Then
      'P = (PSIZE / CUMSIZ(NDC - 1)) * Cum(NDC - 1)
      Alpha = Log(Cum(NDC - 2) / Cum(NDC - 1)) / Log(CUMSIZ(NDC - 2) / CUMSIZ(NDC - 1))
      P = Cum(NDC - 1) * (PSIZE / CUMSIZ(NDC - 1)) ^ Alpha
    Else
      P = 0#
    End If
  Else
    If Cum(I) > 0.999999 And Cum(I + 1) > 0.999999 Then
      P = 1!
      Exit Sub
    End If
    If (Cum(I) <= 0.000001) Then
      P = 0!
      Exit Sub
    End If
    If (Cum(I) >= 0.999999) Then I = I + 1
    If (Cum(I + 1) <= 0.000001) Then I = I - 1
    XI = Log(PSIZE / CUMSIZ(I + 1)) / Log(CUMSIZ(I) / CUMSIZ(I + 1))
    ETA = (ALGR(Cum(I)) / ALGR(Cum(I + 1))) ^ XI
    P = 1! - (1! - Cum(I + 1)) ^ ETA
  End If
  End Sub


Function ALGR(X As Single)
'*************************
  ALGR = Log(1! / (1! - X))
End Function

