Shared data for

Close range vocal interaction in the common marmoset (Callithrix Jacchus)
by Landman et al., 2019

This material consists of: 1. wave data, 2. annotations, and 3. Matlab code.

1.Wave data
To get the wave data, download and unzip the following: pair1.zip, pair2.zip, pair3.zip, pair4.zip, pair5.zip, pair6.zip, pair7.zip, pair8.zip, pair9.zip, pair10.zip

10 pairs of animals (cage mates) were recorded. In each pair, one animal is 'animal1' and the other animal is 'animal2'. Each animal wore a jacket with a voice recorder in it. The wave files are named like this:

Pair [1-10] _ animal [1 or 2] _ [condition] .wav

.. where [condition] is either 'together', 'animal1out' or 'animal2out', corresponding to whether they were together in the home cage, or whether animal1 or animal2 was outside the home cage. Multiple sessions were run for each pair. The wave files are concatenations of all segments recorded in that condition

Examples:

Filename 'pair4_animal1_animal1out.wav'
Contents: All the audio from the recorder that animal 1 in pair 4 was wearing when animal 1 himself was outside the home cage

Filename: 'pair7_animal2_together.wav'
Contents: All the audio from the recorder that animal 2 in pair 7 was wearing when they were together in the home cage

2.Annotations
To get the annotations, download and unzip 'annotations.zip'. In it, you will find the file 'annotations.mat' which is a Matlab file that contains a 10x3 structure called 'dataset' which has all our annotations belonging to the wave files

dataset(pair, condition), where

condition=1 means Together

condition=2 means animal2 out

condition=3 means animal1 out

Contents example:

dataset(1,1)

ans =

struct with fields:

  animal1_ts: [447×1 double]
  animal2_ts: [249×1 double]
animal1_tstp: [447×1 double]
animal1_type: {1×447 cell}
animal2_tstp: [249×1 double]
animal2_type: {1×249 cell}
animal1_name: 'Sailor'
animal2_name: 'Setta'
 animal1_sex: 'm'
 animal2_sex: 'f'
   others_ts: [1897×1 double]
 others_tstp: [1897×1 double]
*_ts=call start time in seconds

*_tstp=call stop time in seconds

*_type= call type

The voice recorders were on one co-habitant pair at a time. 'others_ts' and 'others_tstp' refer to calls from animals in other cages that could be heard in the background of each voice recorder. Call type was not determined for these calls (only start and stop time)

Call types:

Type abbreviations:

tr = trill

ph = phee

trph = trillphee

tw = twitter

chi = chirp

cha = chatter

ek = ek

ts = tsik

pe = peep

ic = squeal / infant cry

ot = unidentified call type

Example usage of annotations and wave data:

Suppose we want to get start times of all the calls when pair 6 was together in the cage.
Combine:

dataset(6,1).animal1_ts,

dataset(6,1).animal2_ts,

dataset(6,1).others_ts

Now suppose we want the audio waves audio of calls from animal 2 in pair 6 when they were together in the cage.
Select the wave file 'pair6\pair6_animal2_together.wav'

The start times of the calls are in dataset(6,1).animal2_ts

The stop times of the calls are in dataset(6,1).animal2_tstp

The call types for these calls are in dataset(6,1).animal2_type

Using this information you can then extract the relevant segments of the wave file in Matlab or any software of your choosing. Be aware the time stamps are in seconds, not in samples.

2.Matlab code
To get the Matlab code, download and unzip Matlab_code.zip

Create one large folder with subfolders for each pair, the annotations and the Matlab code, like this

MyFolder\pair1
MyFolder\pair2
MyFolder\pair3
MyFolder\pair4
MyFolder\pair5
MyFolder\pair6
MyFolder\pair7
MyFolder\pair8
MyFolder\pair9
MyFolder\pair10
MyFolder\annotations (contains 'annotations.mat')
MyFolder\Matlab_code (contains Matlab scripts)

To run the code, include the entire folder+subfolders in your Matlab path and make 'Matlab_code' your current directory.

Paper_analysis_call_counts.m

This script calculates call counts and 'call-triggered average'. It generates the figures 2 and 3 from the paper and csv files for analysis in SPSS.

Paper_analysis_waves_and_spectrograms.m

This script extracts trill call wave data, calculates spectrograms and fundamental frequencies. It saves waves and spectrograms to the folders of the corresponding pairs, generates figures and a csv file.
The storage of waves and spectrograms requires approx 5Gb of disk space

The variables 'specs_and_waves' and 'mean_specs' in the beginning of this script control what the script does. If 'specs and waves' is set to 1, it will output spectrograms to your hard drive. This must be run once to generate the data for 'mean_specs'. 'mean_specs' calculates mean spectrograms, fundamental frequency and creates figures. 

