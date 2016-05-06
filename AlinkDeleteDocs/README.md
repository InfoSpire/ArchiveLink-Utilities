# Mass Removal of Links or Links and Content

## Requirements
+ Netweaver ABAP Stack &ge; 4.6D
+ Developer Access and Authorization

## Description
This utility program deletes ArchiveLink entries from the ArchiveLink data tables(TOAXX) based on selection criteria. In addition to removing the records from the tables, it can also remove the content objects from the repository on which they are stored.

## Use Cases
+ After a system refresh (through a system restore) of a production system to a lower environment, it is recommended that data be cleansed in order to remove risk of inappropriate access to productive data and/or removal of erroneous data.

## Installation
At a minimum the installation of the _**Core**_ components listed below is required. If desired, an optional installation step can be taken in order to create a custom transaction code to allow the utility to be called directly.

### Core
1. Execute TC _**SE80**_.
2. Find or Create a package "_**ZBC_ALINKUTILS**_".
3. Create a new program with the name "_**ZBC_ALINK_DELETE_DOCS**_" in package "_**ZBC_ALINKUTILS**_".
4. Copy the contents of the file ["_**zbc_alink_delete_docs.abap**_"](../blob/master/AlinkDeleteDocs/ZBC_ALINK_DELETE_DOCS.abap) to the newly created program.
5. Save and Activate the program.

### Optional (Z-TCode)
1. Execute TC _**SE80**_.
2. Find or Create a package "_**ZBC_ALINKUTILS**_".
3. Create a new transaction code with the name "_**ZBC_DALINK**_" and assign the program "_**ZBC_ALINK_DELETE_DOCS**_" as the target.

## Usage ##
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
		<img src="(../blob/master/Wiki_Resources/SAPUI/sapui_execute_icon_whitebg.png)" alt="Execute"> Execute
	</td>
    <td><img src="../blob/master/Wiki_Resources/AlinkChangeOAC3/images/Test_Selection_Screen1.png" width=550px alt="Screen 1"></td>
  </tr>
  <tr>
    <td>Result:</td>
    <td><img src="../blob/master/Wiki_Resources/AlinkChangeOAC3/images/Test_Selection_Screen2.png" width=550px alt="Screen 2"></td>
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
		<img src="../blob/master/Wiki_Resources/SAPUI/sapui_execute_icon_whitebg.png" alt="Execute"> Execute
	</td>
    <td><img src="../blob/master/Wiki_Resources/AlinkChangeOAC3/images/Update_Selection_Screen1.png" width=550px alt="Screen 1"></td>
  </tr>
  <tr>
    <td>Result:</td>
    <td><img src="../blob/master/Wiki_Resources/AlinkChangeOAC3/images/Update_Selection_Screen2.png" width=550px alt="Screen 2"></td>
  </tr>
</table>

## Additional

## License
The Apache License v2.0 is used for all development objects.<br>
[Apache License](../blob/master/AlinkChangeOAC3/LICENSE)