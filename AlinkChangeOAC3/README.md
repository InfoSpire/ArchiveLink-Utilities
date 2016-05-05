# Mass Change of "_Links for Content Repositories_" #

## Requirements ##
+ Netweaver ABAP Stack &ge; 4.6D
+ Developer Access and Authorization

## Description ##
This utility allows ArchiveLink Administrators to perform a mass change of the configuration that is typically managed through Transaction Code (TC) _**OAC3**_.

## Use Cases ##
+ A business or technical event has necessitated the change of the Content Repository ID for a large block of _**Linkages**_.

## Installation ##
At a minimum the installation of the _**Core**_ components listed below is required. If desired, an optional installation step can be taken in order to create a custom transaction code to allow the utility to be called directly.

### Core ###
1. Execute TC _**SE80**_.
2. Find or Create a package "_**ZBC_ALINKUTILS**_".
3. Create a new program with the name "_**ZBC_ALINK_CHANGE_OAC3**_" in package "_**ZBC_ALINKUTILS**_".
4. Copy the contents of the file ["_**zbc_alink_change_oac3.abap**_"](https://github.com/InfoSpire/ArchiveLink-Utilities/blob/master/AlinkChangeOAC3/ZBC_ALINK_CHANGE_OAC3.abap) to the newly created program.
5. Save and Activate the program.

### Optional (Z-TCode) ###
1. Execute TC _**SE80**_.
2. Find or Create a package "_**ZBC_ALINKUTILS**_".
3. Create a new transaction code with the name "_**ZBC_OAC3**_".
<table>
  <tr>
    <th>Step</th>
    <th>Screenshot</th>
  </tr>
</table>

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
					<li>Cont. Rep. -New</li>
				</ul>
			</li>
			<li>Run Mode
				<ul>
					<li>_**Test Selection**_</li>
				</ul>
			</li>
			<li><img src="https://github.com/InfoSpire/ArchiveLink-Utilities/blob/master/Wiki_Resources/SAPUI/sapui_execute_icon_whitebg.png" alt="Execute"> Execute</li>
		</ol>
	</td>
    <td><img src="https://github.com/InfoSpire/ArchiveLink-Utilities/blob/master/Wiki_Resources/AlinkChangeOAC3/images/Test_Selection_Screen1.png" alt="Screen 1" width=200 height=200></td>
  </tr>
  <tr>
    <td>Result:</td>
    <td><img src="https://github.com/InfoSpire/ArchiveLink-Utilities/blob/master/Wiki_Resources/AlinkChangeOAC3/images/Test_Selection_Screen2.png" alt="Screen 2"></td>
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
					<li>Cont. Rep. -New</li>
				</ul>
			</li>
			<li>Run Mode
				<ul>
					<li>_**Update Entries**_</li>
				</ul>
			</li>
			<li><img src="https://github.com/InfoSpire/ArchiveLink-Utilities/blob/master/Wiki_Resources/SAPUI/sapui_execute_icon_whitebg.png" alt="Execute"> Execute</li>
		</ol>
	</td>
    <td><img src="https://github.com/InfoSpire/ArchiveLink-Utilities/blob/master/Wiki_Resources/AlinkChangeOAC3/images/Update_Selection_Screen1.png" alt="Screen 1"></td>
  </tr>
  <tr>
    <td>Result:</td>
    <td><img src="https://github.com/InfoSpire/ArchiveLink-Utilities/blob/master/Wiki_Resources/AlinkChangeOAC3/images/Update_Selection_Screen2.png" alt="Screen 2"></td>
  </tr>
</table>

## Additional ##

## License ##
The Apache License v2.0 is used for all development objects.
[Apache License](../blob/master/AlinkChangeOAC3/LICENSE)