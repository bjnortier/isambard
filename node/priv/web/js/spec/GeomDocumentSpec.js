describe("GeomDocument", function() {
    
    var doc;
    beforeEach(function() {
        doc = new GeomDocument();
    });

    it("should be empty on creation", function() {
        expect(doc.rootNodes.length).toEqual(0);
    });

    it("can accept and reject root nodes", function() {
        
        var node1 = new GeomNode({type: "sphere"});
        var node2 = new GeomNode({type: "cuboid"});
        doc.add(node1);
        doc.add(node2);
        expect(doc.rootNodes.length).toEqual(2);
        expect(doc.rootNodes[0]).toEqual(node1);
        expect(doc.rootNodes[1]).toEqual(node2);

        doc.remove(node1);
        expect(doc.rootNodes.length).toEqual(1);
        expect(doc.rootNodes[0]).toEqual(node2);

        doc.remove(node2);
        expect(doc.rootNodes.length).toEqual(0);

    });

});