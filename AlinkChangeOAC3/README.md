# Mass Change of "_Links for Content Repositories_"

## Description
This utility allows SAP ArchiveLink&#174; Administrators to perform a mass change of the configuration that is typically managed through Transaction Code (TC) _**OAC3**_.

## Use Cases
+ A business or technical event has necessitated the change of the Content Repository ID for a large block of _**Linkages**_.

## Requirements
+ Netweaver&#174; ABAP&#174; Stack &ge; 7.0 (Note: not tested with lower releases, but may work.)
+ Developer Access and Authorization

## Installation
At a minimum the installation of the _**Core**_ components listed below is required. If desired, an optional installation step can be taken in order to create a custom transaction code to allow the utility to be called directly.

### Core
1. Execute TC _**SE80**_.
<table>
	<tr>
		<td><img src="../Wiki_Resources/General/images/sapui_exec_tc_SE80.png" alt="Screen 1"></td>
	</tr>
</table>
2. Find or Create a package "_**ZBC_ALINKUTILS**_".
<table>
	<tr>
		<td>Find or Create:</td>
		<td><img src="../Wiki_Resources/General/images/sapui_dev_find_package.png?raw=true" alt="Screen 1"></td>
	</tr>
	<tr>
		<td>Create Step 1:</td>
		<td><img src="../Wiki_Resources/General/images/sapui_dev_create_package_001.png?raw=true" alt="Screen 1"></td>
	</tr>
	<tr>
		<td>Create Step 2:</td>
		<td><img src="../Wiki_Resources/General/images/sapui_dev_create_package_002.png?raw=true" alt="Screen 1"></td>
	</tr>
</table>
3. Create a new program with the name "_**ZBC_ALINK_CHANGE_OAC3**_" in package "_**ZBC_ALINKUTILS**_".
<table>
	<tr>
		<th>Step</th>
		<th>Screenshot</th>
	</tr>
	<tr>
		<td>1</td>
		<td><img src="../Wiki_Resources/AlinkChangeOAC3/images/sapui_dev_create_program_ZBC_ALINK_CHANGE_OAC3_001.png?raw=true"  alt="Screen 1"></td>
	</tr>
	<tr>
		<td>2</td>
		<td><img src="../Wiki_Resources/AlinkChangeOAC3/images/sapui_dev_create_program_ZBC_ALINK_CHANGE_OAC3_002.png?raw=true" alt="Screen 1"></td>
	</tr>
</table>
4. Copy the contents of the file ["_**ZBC_ALINK_CHANGE_OAC3.abap**_"](ZBC_ALINK_CHANGE_OAC3.abap) to the newly created program.
5. ![alt text](../Wiki_Resources/SAPUI/sapui_save_icon_whitebg.png "Save") Save and ![alt text](../Wiki_Resources/SAPUI/sapui_activate_icon_whitebg.png "Activate") Activate the program. 

### Optional (Z-TCode)
1. Execute TC _**SE80**_.
2. Find or Create a package "_**ZBC_ALINKUTILS**_".
3. Create a new transaction code with the name "_**ZBC_OAC3**_" and assign the program _**ZBC_ALINK_CHANGE_OAC3**_ as the target.

## Usage
There are two modes in which the utility can be executed _**Test**_ and _**Update**_.
+ Test Selection
<table>
  <tr>
    <th>Step</th>
    <th>Screenshot</th>
  </tr>
  <tr>
    <td>Add Criteria:
		<ol>
			<li>Selection
				<ul>
					<li>Object Type</li>
					<li>Document Type</li>
					<li>Link Status Archive</li>
					<li>Cont. Rep. - Old</li>
				</ul>
			</li>
			<li>Update
				<ul>
					<li>Cont. Rep. - New</li>
				</ul>
			</li>
			<li>Run Mode
				<ul>
					<li>_**Test Selection**_</li>
				</ul>
			</li>
		</ol>
		<img src="../Wiki_Resources/SAPUI/sapui_execute_icon_whitebg.png" alt="Execute"> Execute
	</td>
    <td><img src="../Wiki_Resources/AlinkChangeOAC3/images/Test_Selection_Screen1.png?raw=true" width=550px alt="Screen 1"></td>
  </tr>
  <tr>
    <td>Result:</td>
    <td><img src="../Wiki_Resources/AlinkChangeOAC3/images/Test_Selection_Screen2.png?raw=true" width=550px alt="Screen 2"></td>
  </tr>
</table>
+ Update Entries
<table>
  <tr>
    <th>Step</th>
    <th>Screenshot</th>
  </tr>
  <tr>
        <td>Add Criteria:
		<ol>
			<li>Selection
				<ul>
					<li>Object Type</li>
					<li>Document Type</li>
					<li>Link Status Archive</li>
					<li>Cont. Rep. - Old</li>
				</ul>
			</li>
			<li>Update
				<ul>
					<li>Cont. Rep. - New</li>
				</ul>
			</li>
			<li>Run Mode
				<ul>
					<li>_**Update Entries**_</li>
				</ul>
			</li>
		</ol>
		<img src="../Wiki_Resources/SAPUI/sapui_execute_icon_whitebg.png" alt="Execute"> Execute
	</td>
    <td><img src="../Wiki_Resources/AlinkChangeOAC3/images/Update_Selection_Screen1.png?raw=true" width=550px alt="Screen 1"></td>
  </tr>
  <tr>
    <td>Result:</td>
    <td><img src="../Wiki_Resources/AlinkChangeOAC3/images/Update_Selection_Screen2.png?raw=true" width=550px alt="Screen 2"></td>
  </tr>
</table>

## Additional

## License
The Apache License v2.0 is used for all development objects.<br>
[Apache License](LICENSE)
