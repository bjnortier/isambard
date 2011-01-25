// A simple program that computes the square root of a number
#include <stdio.h>
#include <iostream>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>


// OpenCASCADE
#include <BRep_Tool.hxx>
#include <BRepLib.hxx>
#include <BRepMesh.hxx>
#include <BRepPrimAPI_MakeCylinder.hxx>
#include <BRepPrimAPI_MakePrism.hxx>
#include <BRepPrimAPI_MakeSphere.hxx>
#include <BRepPrimAPI_MakeBox.hxx>
#include <gp.hxx>
#include <gp_Pnt.hxx>
#include <TopExp_Explorer.hxx>
#include <TopoDS.hxx>
#include <TopoDS_Edge.hxx>
#include <TopoDS_Face.hxx>
#include <Poly_Triangulation.hxx>

// Spirit
#include "json_spirit.h"

// Project
#include "WorkerConfig.h"

using namespace std;
using namespace json_spirit;

// TODO: Tesselate in parallel
// TODO: compress 1.00000000 into 1.0 etc. Also reduce precision


map< string, TopoDS_Shape > shapes = map< string, TopoDS_Shape >();

mValue tesselate(string id) {
  TopoDS_Shape shape = shapes[id];
  BRepMesh().Mesh(shape, 0.0125);

  mArray vertex_array;
  mArray indices;
  mArray positions;
  mArray normalArr;

  map< int, gp_Vec > normals = map< int, gp_Vec >();
  map< int, vector< int > > connected_triangles = map< int, vector< int > >();

  TopExp_Explorer Ex; 
  int index_offset = 0;
  for (Ex.Init(shape,TopAbs_FACE); Ex.More(); Ex.Next()) { 

    TopoDS_Face Face = TopoDS::Face(Ex.Current());
    TopLoc_Location Location = TopLoc_Location();
    Handle(Poly_Triangulation) facing = BRep_Tool().Triangulation(Face,Location);

    for (int i = 1; i <= facing->NbTriangles(); ++i) {
      Poly_Triangle triangle = facing->Triangles().Value(i);
      Standard_Integer index1, index2, index3;
      triangle.Get(index1, index2, index3);

      // Step 1 - caluclate the normals of the triangles
      gp_Pnt vertex1 = facing->Nodes().Value(index1);
      gp_Pnt vertex2 = facing->Nodes().Value(index2);
      gp_Pnt vertex3 = facing->Nodes().Value(index3);

      gp_Vec vec12 = gp_Vec(vertex1, vertex2);
      gp_Vec vec23 = gp_Vec(vertex2, vertex3);


      gp_Vec cross_product = vec12.Crossed(vec23);

      double cross_magnitude = cross_product.Magnitude();
      if (cross_magnitude > 0) {
        gp_Vec normal = cross_product.Normalized();
        normals[i] = normal;
      }

      // Step 2 - created connected lookup
      connected_triangles[index1].push_back(i);
      connected_triangles[index2].push_back(i);
      connected_triangles[index3].push_back(i);

      indices.push_back(index1 - 1 + index_offset);
      indices.push_back(index2 - 1 + index_offset);
      indices.push_back(index3 - 1 + index_offset);
    }

    for (int i = 1; i <= facing->NbNodes(); ++i) {
      gp_Pnt vertex = facing->Nodes().Value(i);
      int count = 0;
      vector< int > connected = connected_triangles[i];
      vector< int >::iterator it;

      gp_Vec avg = gp_Vec(0.0, 0.0, 0.0);
      for(it = connected.begin(); it != connected.end(); ++it) {
        gp_Vec normal = normals[(*it)];
        avg += normal;
        ++count;
      }
      if (count > 0) {
        avg /= (double)count;
      }

      positions.push_back(vertex.X());
      positions.push_back(vertex.Y());
      positions.push_back(vertex.Z());

      normalArr.push_back(avg.X());
      normalArr.push_back(avg.Y());
      normalArr.push_back(avg.Z());
    }

    index_offset += facing->NbNodes();

  }

  mObject result;
  result["primitive"] = "triangles";
  result["positions"] = positions;
  result["normals"] = normalArr;
  result["indices"] = indices;
      
  return result;
}

double get_double(mValue value) {
  if (value.type() == int_type) {
    return (double)value.get_int();
  } else {

    return value.get_real();
  }
}

mValue create_sphere(string id, map< string, mValue > geometry) {
  map< string, mValue > parameters = geometry["parameters"].get_obj();
  mValue radius = parameters["radius"];
  if (!radius.is_null() && ((radius.type() == real_type) || (radius.type() == int_type))) {
    TopoDS_Shape shape = BRepPrimAPI_MakeSphere(get_double(radius)).Shape();
    shapes[id] = shape;
    return tesselate(id);
  }
  return  mValue("invalid geometry parameters");
}

mValue create_cuboid(string id, map< string, mValue > geometry) {
  map< string, mValue > parameters = geometry["parameters"].get_obj();
  mValue width = parameters["width"];
  mValue depth = parameters["depth"];
  mValue height = parameters["height"];
  if (!width.is_null() && ((width.type() == real_type) || (width.type() == int_type))
      &&
      !depth.is_null() && ((depth.type() == real_type) || (depth.type() == int_type))
      &&
      !height.is_null() && ((height.type() == real_type) || (height.type() == int_type))) {
    
    TopoDS_Shape shape = BRepPrimAPI_MakeBox(get_double(width), 
                                             get_double(depth), 
                                             get_double(height)).Shape();
    shapes[id] = shape;
    return tesselate(id);
  }
  return  mValue("invalid geometry parameters");
}

mValue create_geometry(string id, map< string, mValue > geometry) {
  mValue geomType = geometry["type"];
  if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("cuboid"))) {
    return create_cuboid(id, geometry);
  } 
  if (!geomType.is_null() && (geomType.type() == str_type) && (geomType.get_str() == string("sphere"))) {
    return create_sphere(id, geometry);
  } 
  return mValue("geometry type not found");
}

int read_exact(unsigned char *buf, int len) {
  int i, got=0;
  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);
  return len;
}

int read_cmd(unsigned char* buf) {
  int len;
  if (read_exact(buf, 4) != 4)
    return(-1);
  len = (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];
  return read_exact(buf, len);
}

int write_exact(const char *buf, int len) {
  int i, wrote = 0;
  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);
  return len;
}

int write_cmd(const char *buf, int len) {
  char size_buf[4];
  size_buf[0] = (len >> 24) & 0xff;
  size_buf[1] = (len >> 16) & 0xff;
  size_buf[2] = (len >> 8) & 0xff;
  size_buf[3] = len & 0xff;
  write_exact(size_buf, 4);
  return write_exact(buf, len);
}

int main (int argc, char *argv[]) {

  unsigned char buf[1024*1024];
  int json_size;

  unsigned char size_buf[4];
  /*while ((json_size = read(0, size_buf,4)) > 0) {

    mObject result;
    result["0"] = mValue(size_buf[0]);
    result["1"] = mValue(size_buf[1]);
    result["2"] = mValue(size_buf[2]);
    result["3"] = mValue(size_buf[3]);

    int len = (size_buf[0] << 24) | (size_buf[1] << 16) | (size_buf[2] << 8) | size_buf[3];

    result["len"] = mValue(len);

    string output = write(result);
    write_cmd(output.c_str(), output.size());*/

    
  while ((json_size = read_cmd(buf)) > 0) {
        
    try {
      mValue value;
      // Set 0-delimiter for conversion to string
      buf[json_size] = 0;
      string json_string = string((char*)buf);
      read(json_string, value);

      if (value.type() == obj_type) {
        map< string, mValue > objMap = mObject(value.get_obj());

        mValue msgType = objMap["type"];
        mValue id = objMap["id"];
        mValue geometry = objMap["geometry"];

        if (!msgType.is_null() && (msgType.type() == str_type) && (msgType.get_str() == string("create"))
            && 
            !id.is_null() && (id.type() == str_type)
            && 
            !geometry.is_null() && (geometry.type() == obj_type)) {
          

          mValue response = create_geometry(id.get_str(), mObject(geometry.get_obj()));
          string output = write(response);
          write_cmd(output.c_str(), output.size());
          continue;
        }
      }
        
      mValue response = mValue("unknown message");
      string output = write(response);
      write_cmd(output.c_str(), output.size());
    } catch (...) {
      mValue response = mValue("exception!");
      string output = write(response);
      write_cmd(output.c_str(), output.size());

    }
      
  }

  return 0;
}
