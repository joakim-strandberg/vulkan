# vulkan

The goal is to create an Ada binding for the Vulkan SDK. The vk.xml file that describes the API can now be parsed. Work is ongoing to interpret the contents and generate the Ada binding.

This is work in progress...

One subgoal has been to implement the parser of the vk.xml-file in SPARK (or use SPARK as much as possible to be able to use the SPARK tools to be able to formally verify the code and to prove it free from run-time errors under the assumption that the RAM memory of the computer does not become exhausted).
