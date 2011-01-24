describe("Command", function() {

    var doc;
    beforeEach(function() {
        doc = new Document();
    });

    it("should modify a document", function() {

        var node = new GeomNode({type: "cuboid"});
        
        var command = new Command(
            function() {
                doc.add(node);
            },
            function() {
                doc.remove(node);
            }
        );
        
        expect(doc.rootNodes).toEqual([]);
        command.do();
        expect(doc.rootNodes).toEqual([node]);
        command.undo();
        expect(doc.rootNodes).toEqual([]);
    });

});