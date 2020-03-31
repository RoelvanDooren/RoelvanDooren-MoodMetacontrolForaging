#! /bin/bash/

@echo off

echo PhD Metacontrol: Leiden University
echo Zsuzsika Sjoerds and Roel van Dooren
echo.
echo Last adjustment on: 2019-02-27 08:17
echo r.van.dooren@fsw.leidenuniv.nl
set expStartTime=%DATE:~9,4%%DATE:~6,2%%DATE:~2,3% %TIME:~0,2%%TIME:~3,2%%TIME:~6,2%
set expStartTime=%expStartTime: =%
echo.
echo Current Time: %expStartTime%
set /p subject_ID= "Enter subject ID: "

:: =============================================================================

:: Counterbalance mood manipulation order
set /a counterbalance=%subject_ID% %% 2
IF %counterbalance% GEQ 1 set condition=excited
IF %counterbalance% == 0 set condition=sad

:: =============================================================================

:: Practice Mood/Arousal grid
python ./affect_grid.py %1 %expStartTime% %subject_ID% example %condition%

:: Practice trial: hybrid visual foraging task
python ./foraging_practice.py %1 %expStartTime% %subject_ID% practice

:: Calibrate eyetracker
::"C:\Program Files (x86)\Tobii\Calibration and Verification Tool\Calibration and Verification Tool.exe"

:: Main experiment: foraging task baseline
:: Note: includes pre-foraging mood/arousal measure and post-foraging Mood/Arousal grid
python ./foraginggame.py %1 %expStartTime% %subject_ID% baseline %condition%

:: Main experiment: Mood induction task
:: Note: includes pre-induction and post-induction Mood/Arousal grids
call "C:\ExperimentData\BP53_2019_Sjoerds_vanDooren\BA2019\moodinduction\Mood.ebs2"

:: Recalibrate the eyetracker
::"C:\Program Files (x86)\Tobii\Calibration and Verification Tool\Calibration and Verification Tool.exe"

:: Main experiment: foraging task main
:: Note: includes pre-foraging Mood/Arousal grid, post-foraging Mood/Arousal grid and a short questionnaire
python ./foraginggame.py %1 %expStartTime% %subject_ID% main %condition%

:: Clean up our experimental folder
del *.pyc

pause
