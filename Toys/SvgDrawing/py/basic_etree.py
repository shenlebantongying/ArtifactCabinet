import xml.etree.ElementTree as ET

root = ET.Element(
    "svg", {"width": "300", "height": "200", "xmlns": r"http://www.w3.org/2000/svg"}
)

ET.SubElement(root, "circle", {"cx": "100", "cy": "100", "r": "50", "fill": "green"})

tree = ET.ElementTree(root)
ET.indent(tree)

tree.write("../output.svg")
