c***************************************************************
c      LE PARAGLIDING v 3.29 "Jardins"
c      Pere Casellas 2010-2026
c      Laboratori d'envol
c      http://www.laboratoridenvol.com
c      pere AT laboratoridenvol DOT com
c      Version experimental 0.1: 2005-02-13
c      Version 0.8: 2010-01-02 "gnuLAB2"
c      Version 0.9: 2010-02-14
c      Version 1.0: 2010-03-07
c      Version 1.02: 2010-04-17 "Annency"
c      Version 1.1: 2010-04-25 "South Africa"
c      Version 1.11: 2010-12-26 "Montseny"
c      Version 1.2: 2011-01-14 "Adrenaline"
c      Version 1.25: 2011-03-20 "Romano"
c      Version 1.4: 2011-04-25 "V-Ribs"
c      Verssion 1.5: 2011-12-08 "HyperLite"
c      Version 2.0: 2012-01-08 "BHL"
c      Version 2.1: 2012-05-27 "BatLite"
c      Version 2.2: 2013-05-05 "Altair"
c      Version 2.21: 2013-07-17 "Fluid Wings"
c      Version 2.23: 2013-08-13 "BHL-2"
c      Version 2.31: 2013-12-31 "BASE"
c      Version 2.35: 2014-04-21 "BASE"
c      Version 2.37: 2015-04-25 "Omsk"
c      Versiom 2.41: 2015-09-20 "Omsk"
c      Version 2.45: 2016-03-12 "Utah"
c      Version 2.50: 2016-05-09 "Utah"
c      Version 2.51: 2016-06-05
c      Version 2.52: 2016-08-18
c      Version 2.52++: 2016-08-27
c      Version 2.60: 2016-12-12 "Les Escaules"
c      Version 2.70: 2018-02-04 "Baldiri"
c      Version 2.73: 2018-05-12 "Baldiri"
c      Version 2.77; 2018-08-28 "Baldiri" 
c      Version 2.80; 2018-10-12 "Baldiri"
c      Version 2.81: 2018-12-24
c      Version 2.85: 2019-01-01
c      Version 2.88: 2019-01-07
c      Version 2.90: 2019-01-13 
c      Version 2.95: 2019-01-20   
c      Version 2.96: 2019-05-07
c      Version 2.99: 2019-06-24
c      Version 3.00: 2020-01-12 "Pirineus"
c      Version 3.02: 2020-01-26 "Pirineus"
c      Version 3.03: 2020-04-13 "Pirineus"
c      Version 3.10: 2020-05-02 "Pirineus"
c      Version 3.11: 2020-09-06 "Pirineus"
c      Version 3.12: 2020-12-15 "Pirineus"
c      Version 3.14: 2020-12-25 "Pirineus"
c      Version 3.15: 2021-01-17 "Canigó"
c      Version 3.16: 2021-08-29 "Z"
c      Version 3.16+: 2021-11-27 "Z"
c      Version 3.17: 2021-12-12 "Z"   
c      Version 3.17+: 2022-01-03 "Z"
c      version 3.18: 2022-02-06 "Vinebre"
c      version 3.19: 2022-05-22 "Vinebre"
c      version 3.20U: 2022-09-01 "Vinebre"
c      version 3.20V: 2022-09-18 "Vinebre"
c      version 3.21T: 2023-01-05 "Gorraptes"
c      version 3.23: 2023-12-11 "Gorraptes"   
c      version 3.23+: 2024-01-07 "Gorraptes" 
c      version 3.24: 2024-07-12 "Ebre"
c      version 3.24a: 2024-09-06 "Ebre"
c      version 3.24a: 2024-12-11 "Pic de Midi de Bigorre"
c      version 3.25 2025-01-10 "Jardins"
c      version 3.27e 2025-12-11 "Jardins"
c      version 3.27+ 2026-01-28 "Jardins"
c      version 3.28 2026-05-01 "Jardins"
c      version 3.29 2026-08-19 "Jardins"
c      FORTRAN fort77/gfortran (GNU/Linux)
c      GNU General Public License 3.0 (http://www.gnu.org)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c       program leparagliding
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!> Run the complete LEparagliding design pipeline.
!!
!! The program reads `leparagliding.txt` and its referenced profile files from
!! the current working directory, calculates the wing geometry, and writes the
!! DXF and text deliverables documented in the project README.
!!
!! @note The numbered include files deliberately share this program's legacy
!!       variable scope and must remain in calculation order.
       program leparagliding
       use leparagliding_procedures
       use leparagliding_domain_model
       use leparagliding_mark_types
       use leparagliding_hvr_config

       include 'main/declarations.inc'
       include 'main/03_initialization.inc'
       include 'main/04_data_reading.inc'
       include 'main/05_graphic_design.inc'
       include 'main/06_airfoil_geometry.inc'
       include 'main/07_panel_development.inc'
       include 'main/08_skin_tension.inc'
       include 'main/09_singular_rib_points.inc'
       include 'main/10_calage.inc'
       include 'main/11_panel_lengths.inc'
       include 'main/12_lines.inc'
       include 'main/14_brakes.inc'
       include 'main/15_colors.inc'
       include 'main/16_internal_ribs.inc'
       include 'main/17_equilibrium.inc'
       include 'main/18_text_output.inc'
       include 'main/19_lines_output.inc'
       include 'main/20_line_labels.inc'
       include 'main/21_3d_dxf.inc'
       include 'main/22_notes.inc'
       include 'main/23_finish.inc'

       end program leparagliding
