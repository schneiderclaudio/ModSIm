Attribute VB_Name = "MdlEztest"
Option Explicit
'DEFINE YOUR VARIABLES
Public X(61) As Single, Y(61) As Single, Y2(61) As Single

 Sub Main()
'      PROGRAM EZTEST
'     **************
'  Program that demonstrates the principles of graph plotting
'  using metgraph.

   Dim I As Integer
'
'   CALCULATE THE VALUES
      For I = 1 To 61
        X(I) = (I - 1) * 0.02
        Y(I) = X(I) * X(I)
        Y2(I) = 1.6 * Y(I)
      Next I
'
'   OPEN METGRAPH
      Call RTAB
'   DISPLAY THE GRAPH
      Call GROUT
'   CLOSE METGRAPH
      Call MGCLOSE
 End Sub
 
Sub DrawPage(I As Integer)
'*************************
'Contains code to draw the graphics pages.
Static ICOLOR As Integer
Select Case I
  Case 1
'   DRAW A BLANK BOX
      Call bbox(100, 100, 700, 500)
'
'      Call CLIPBOX
'   SCALE THE AXES AND SET THE SCALING FOR SUBSEQUENT PLOTS
      Call LINAX(0#, 1#, 10, 4.1, 1, "Y", "Y")
      Call LINAX(0#, 1#, 10, 4.1, 2, "Y", "Y")
'
'   LABEL THE AXES
      Call LABAX("X axis~Sup~ever 2", 17, " ", 1, 13, 1)
      Call LABAX("Y axis`sub1`more 2", 18, " ", 1, 13, 2)
      
'   CLIP THE GRAPH AT THE BOX
      Call CLIPBOX
'
'   PLOT THE GRAPH
      Call PLGRPH(X, Y, 61, "LINTR", "LINTR")
      Call CHANGECOL(ICOLOR)
      Call PLGRPH(X, Y2, 61, "LINTR", "LINTR")
      
      Call CLIPOFF

End Select
End Sub
