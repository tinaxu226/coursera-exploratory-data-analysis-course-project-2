## secTidySet.txt description

This data file contains 180 rows and 68 columns. Each row has averaged variables for each subject and each activity.
Only all the variables estimated from mean and standard deviation in the tidy set were kept.

    mean(): Mean value
    std(): Standard deviation

The data were averaged based on subjectId and activityId.

Subject column is numbered sequentially from 1 to 30. Activity column has 6 types as listed below assigned with a numeric activityId.

   1. WALKING
   2. WALKING_UPSTAIRS
   3. WALKING_DOWNSTAIRS
   4. SITTING
   5. STANDING
   6. LAYING

The tidy data set contains 6 rows (averaged based on activity) and 68 columns (66 variables and activity labels).

### variables 

All measurements are floating-point values, normalised and bounded within [-1,1].
The measurements are classified in two domains:
Time-domain signals

    Average time-domain body acceleration in the X, Y and Z directions:
        timeDomainBodyAccelerometerMeanX
        timeDomainBodyAccelerometerMeanY
        timeDomainBodyAccelerometerMeanZ

    Standard deviation of the time-domain body acceleration in the X, Y and Z directions:
        timeDomainBodyAccelerometerStandardDeviationX
        timeDomainBodyAccelerometerStandardDeviationY
        timeDomainBodyAccelerometerStandardDeviationZ

    Average time-domain gravity acceleration in the X, Y and Z directions:
        timeDomainGravityAccelerometerMeanX
        timeDomainGravityAccelerometerMeanY
        timeDomainGravityAccelerometerMeanZ

    Standard deviation of the time-domain gravity acceleration in the X, Y and Z directions:
        timeDomainGravityAccelerometerStandardDeviationX
        timeDomainGravityAccelerometerStandardDeviationY
        timeDomainGravityAccelerometerStandardDeviationZ

    Average time-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions:
        timeDomainBodyAccelerometerJerkMeanX
        timeDomainBodyAccelerometerJerkMeanY
        timeDomainBodyAccelerometerJerkMeanZ

    Standard deviation of the time-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions:
        timeDomainBodyAccelerometerJerkStandardDeviationX
        timeDomainBodyAccelerometerJerkStandardDeviationY
        timeDomainBodyAccelerometerJerkStandardDeviationZ

    Average time-domain body angular velocity in the X, Y and Z directions:
        timeDomainBodyGyroscopeMeanX
        timeDomainBodyGyroscopeMeanY
        timeDomainBodyGyroscopeMeanZ

    Standard deviation of the time-domain body angular velocity in the X, Y and Z directions:
        timeDomainBodyGyroscopeStandardDeviationX
        timeDomainBodyGyroscopeStandardDeviationY
        timeDomainBodyGyroscopeStandardDeviationZ

    Average time-domain body angular velocity jerk (derivation of the angular velocity in time) in the X, Y and Z directions:
        timeDomainBodyGyroscopeJerkMeanX
        timeDomainBodyGyroscopeJerkMeanY
        timeDomainBodyGyroscopeJerkMeanZ

    Standard deviation of the time-domain body angular velocity jerk (derivation of the angular velocity in time) in the X, Y and Z directions:
        timeDomainBodyGyroscopeJerkStandardDeviationX
        timeDomainBodyGyroscopeJerkStandardDeviationY
        timeDomainBodyGyroscopeJerkStandardDeviationZ

    Average and standard deviation of the time-domain magnitude of body acceleration:
        timeDomainBodyAccelerometerMagnitudeMean
        timeDomainBodyAccelerometerMagnitudeStandardDeviation

    Average and standard deviation of the time-domain magnitude of gravity acceleration:
        timeDomainGravityAccelerometerMagnitudeMean
        timeDomainGravityAccelerometerMagnitudeStandardDeviation

    Average and standard deviation of the time-domain magnitude of body acceleration jerk (derivation of the acceleration in time):
        timeDomainBodyAccelerometerJerkMagnitudeMean
        timeDomainBodyAccelerometerJerkMagnitudeStandardDeviation

    Average and standard deviation of the time-domain magnitude of body angular velocity:
        timeDomainBodyGyroscopeMagnitudeMean
        timeDomainBodyGyroscopeMagnitudeStandardDeviation

    Average and standard deviation of the time-domain magnitude of body angular velocity jerk (derivation of the angular velocity in time):
        timeDomainBodyGyroscopeJerkMagnitudeMean
        timeDomainBodyGyroscopeJerkMagnitudeStandardDeviation

Frequency-domain signals

    Average frequency-domain body acceleration in the X, Y and Z directions:
        frequencyDomainBodyAccelerometerMeanX
        frequencyDomainBodyAccelerometerMeanY
        frequencyDomainBodyAccelerometerMeanZ

    Standard deviation of the frequency-domain body acceleration in the X, Y and Z directions:
        frequencyDomainBodyAccelerometerStandardDeviationX
        frequencyDomainBodyAccelerometerStandardDeviationY
        frequencyDomainBodyAccelerometerStandardDeviationZ

    Weighted average of the frequency components of the frequency-domain body acceleration in the X, Y and Z directions:
        frequencyDomainBodyAccelerometerMeanFrequencyX
        frequencyDomainBodyAccelerometerMeanFrequencyY
        frequencyDomainBodyAccelerometerMeanFrequencyZ

    Average frequency-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions:
        frequencyDomainBodyAccelerometerJerkMeanX
        frequencyDomainBodyAccelerometerJerkMeanY
        frequencyDomainBodyAccelerometerJerkMeanZ

    Standard deviation of the frequency-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions:
        frequencyDomainBodyAccelerometerJerkStandardDeviationX
        frequencyDomainBodyAccelerometerJerkStandardDeviationY
        frequencyDomainBodyAccelerometerJerkStandardDeviationZ

    Weighted average of the frequency components of the frequency-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions:
        frequencyDomainBodyAccelerometerJerkMeanFrequencyX
        frequencyDomainBodyAccelerometerJerkMeanFrequencyY
        frequencyDomainBodyAccelerometerJerkMeanFrequencyZ

    Average frequency-domain body angular velocity in the X, Y and Z directions:
        frequencyDomainBodyGyroscopeMeanX
        frequencyDomainBodyGyroscopeMeanY
        frequencyDomainBodyGyroscopeMeanZ

    Standard deviation of the frequency-domain body angular velocity in the X, Y and Z directions:
        frequencyDomainBodyGyroscopeStandardDeviationX
        frequencyDomainBodyGyroscopeStandardDeviationY
        frequencyDomainBodyGyroscopeStandardDeviationZ

    Weighted average of the frequency components of the frequency-domain body angular velocity in the X, Y and Z directions:
        frequencyDomainBodyGyroscopeMeanFrequencyX
        frequencyDomainBodyGyroscopeMeanFrequencyY
        frequencyDomainBodyGyroscopeMeanFrequencyZ

    Average, standard deviation, and weighted average of the frequency components of the frequency-domain magnitude of body acceleration:
        frequencyDomainBodyAccelerometerMagnitudeMean
        frequencyDomainBodyAccelerometerMagnitudeStandardDeviation
        frequencyDomainBodyAccelerometerMagnitudeMeanFrequency

    Average, standard deviation, and weighted average of the frequency components of the frequency-domain magnitude of body acceleration jerk (derivation of the acceleration in time):
        frequencyDomainBodyAccelerometerJerkMagnitudeMean
        frequencyDomainBodyAccelerometerJerkMagnitudeStandardDeviation
        frequencyDomainBodyAccelerometerJerkMagnitudeMeanFrequency

    Average, standard deviation, and weighted average of the frequency components of the frequency-domain magnitude of body angular velocity:
        frequencyDomainBodyGyroscopeMagnitudeMean
        frequencyDomainBodyGyroscopeMagnitudeStandardDeviation
        frequencyDomainBodyGyroscopeMagnitudeMeanFrequency

    Average, standard deviation, and weighted average of the frequency components of the frequency-domain magnitude of body angular velocity jerk (derivation of the angular velocity in time):
        frequencyDomainBodyGyroscopeJerkMagnitudeMean
        frequencyDomainBodyGyroscopeJerkMagnitudeStandardDeviation
        frequencyDomainBodyGyroscopeJerkMagnitudeMeanFrequency
