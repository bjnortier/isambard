// A simple program that computes the square root of a number
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// OpenCASCADE
#include <BRep_Tool.hxx>
#include <BRepLib.hxx>
#include <BRepMesh.hxx>
#include <BRepPrimAPI_MakeCylinder.hxx>
#include <BRepPrimAPI_MakePrism.hxx>
#include <BRepPrimAPI_MakeSphere.hxx>
#include <gp.hxx>
#include <gp_Pnt.hxx>
#include <TopExp_Explorer.hxx>
#include <TopoDS.hxx>
#include <TopoDS_Edge.hxx>
#include <TopoDS_Face.hxx>
#include <Poly_Triangulation.hxx>

// Project
#include "TutorialConfig.h"



int main (int argc, char *argv[]) {
  TopoDS_Shape bottle = BRepPrimAPI_MakeSphere(1.0).Shape();//MakeBottle(1.0, 1.0, 1.0);
  BRepMesh().Mesh(bottle, 0.0125);

  TopExp_Explorer Ex; 
  for (Ex.Init(bottle,TopAbs_FACE); Ex.More(); Ex.Next()) { 

    TopoDS_Face Face = TopoDS::Face(Ex.Current());

    TopLoc_Location Location = TopLoc_Location();
    
    Handle(Poly_Triangulation) facing = BRep_Tool().Triangulation(Face,Location);
    cout << "number of nodes: " << facing->NbTriangles() << "\n";

    for (int i = 1; i <= facing->NbTriangles(); ++i) {
      Poly_Triangle triangle = facing->Triangles().Value(i);
      Standard_Integer index1, index2, index3;
      triangle.Get(index1, index2, index3);
      //cout << index1 << ":" << index2 << ":" << index3 << "\n";

      gp_Pnt vertex1 = facing->Nodes().Value(index1);
      cout << vertex1.X() << ":" << vertex1.Y() << ":" << vertex1.Z() << "\n";
    }
    
  } 

  return 0;
}
