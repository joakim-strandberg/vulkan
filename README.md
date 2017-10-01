# Ada binding to the Vulkan STD

To use the Ada binding simply with the vulkan.gpr file in your project.

It is possible to query the Vulkan API for information on all the GPU:s installed on the computer. It may even be possible to use the vulkan API in its current state for drawing graphics, but is yet untested.

For examples using the Ada binding consult the /examples directory.

Open the file vk_xml_parser.gpr with the GNAT Programming studio yo view the code that parses the vk.xml file and auto-generates the Ada binding to the Vulkan API. The application is written in Ada 2012 and demonstrates how to take advantage of the new subpool features introduced in the Ada 2012 standard and uses Brad Moore's Deepend.
