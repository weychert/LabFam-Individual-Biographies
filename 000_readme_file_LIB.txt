# LIB-Code
The R codes to prepare LabFam harmonized histories is available at … Code prepared using R [64-bit] Version 4.4.0 on Windows 11, and Rstudio to harmonize data from five household panel surveys. Current version: CPF ver. 1.52 (13.10.2023) Info at https://labfam.uw.edu.pl/labfam-individual-biographies-lib/

## About LIB
 LIB is an open science project which aims at harmonising family and employment histories from longitudinal databases from several countries. We construct spell data for individuals along three different life dimensions: 
fertility, which records the number and timing of births 
partnership, which records the timing of union formation and union dissolution
employment, which collects data on employment spells and characteristics of the jobs held, when available. 
This information is drawn from available longitudinal surveys, utilizing their panel components, calendar modules and retrospective questionnaires. 
Project's website:https://labfam.uw.edu.pl/labfam-individual-biographies-lib/
GitHub: www.github.com/lib
LIB is managed by: Ewa Weychert, Beata Osiewalska, Lucas van der Velde, Anna Matysiak

## How to start
1. Read the Manual.
2. Download LIB-Code from the www.github.com/lib
3. Unzip Code_for_LIB_v1.0 with LIB R scripts
4. Run “00_setting_work_space.R” to create folder for input datafiles
4. Copy the original input datafiles (e.g. from SOEP) to specific folders (e.g. "C:\Code_for_LIB_v1.0\LIB_data\Germany").
5. Select the country and biography scope
6. Run "01_data_selector.R"
MIT License

Copyright (c) [2024] LIB Ewa Weychert, Beata Osiewalska, Lucas van der Velde, Anna Matysiak 

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
