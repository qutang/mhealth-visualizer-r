#' @name SensorData.filter.bessel
#' @title Apply low pass bessel filter to the input sensor data frame
#' @export
#' @import signal matlab plyr
SensorData.filter.bessel = function(sensorData, breaks, Fs, Fc, order){
  # real bessel filter design based on the implementation of matlab
  armaCoeffs = .besself(Fs = Fs, Fc = Fc, order = order)

  nCols = ncol(sensorData)
  if(missing(breaks) || is.null(breaks)){
    sensorData$breaks = .SummaryData.getBreaks(ts = sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER])
  }else{
    sensorData$breaks = .SummaryData.getBreaks(ts = sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER], breaks = breaks)
  }
  result = ddply(sensorData,.(breaks), function(rows){
    colFilter = colwise(.fun = function(x, filt){
      filtered = filter(filt, x)
      return(as.numeric(filtered))
    }, filt = armaCoeffs)
    filteredValue = colFilter(rows[,2:nCols])
    return(filteredValue)
  })
  names(result)[1] = MHEALTH_CSV_TIMESTAMP_HEADER
  for(i in 2:nCols){
    names(result)[i] = paste("BESSEL", names(result)[i],sep="_")
  }
  result[MHEALTH_CSV_TIMESTAMP_HEADER] = as.POSIXct(result[[MHEALTH_CSV_TIMESTAMP_HEADER]])
  result$breaks = result[[MHEALTH_CSV_TIMESTAMP_HEADER]]
  result[MHEALTH_CSV_TIMESTAMP_HEADER] = sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER]
  return(result)
}

#' @name SensorData.filter.butterworth
#' @title Apply high pass butterworth filter to the input sensor data frame
#' @export
#' @import signal matlab plyr
SensorData.filter.butterworth = function(sensorData, breaks, Fs, Fc, order){
  nyquist=Fs/2;
  coeffs =butter(order,Fc/nyquist,'high');
  nCols = ncol(sensorData)

  if(missing(breaks) || is.null(breaks)){
    sensorData$breaks = .SummaryData.getBreaks(ts = sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER])
  }else{
    sensorData$breaks = .SummaryData.getBreaks(ts = sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER], breaks = breaks)
  }
  result = plyr::ddply(sensorData,.(breaks), function(rows){
    colFilter = colwise(.fun = function(x, filt, a){
      filtered = filter(filt, a, x)
      return(as.numeric(filtered))
    }, filt = coeffs$b, a = coeffs$a)
    filteredValue = colFilter(rows[,2:nCols])
    return(filteredValue)
  })
  names(result)[1] = MHEALTH_CSV_TIMESTAMP_HEADER
  for(i in 2:nCols){
    names(result)[i] = paste("BUTTERWORTH", names(result)[i],sep="_")
  }
  result[MHEALTH_CSV_TIMESTAMP_HEADER] = as.POSIXct(result[[MHEALTH_CSV_TIMESTAMP_HEADER]])
  result$breaks = result[[MHEALTH_CSV_TIMESTAMP_HEADER]]
  result[MHEALTH_CSV_TIMESTAMP_HEADER] = sensorData[,MHEALTH_CSV_TIMESTAMP_HEADER]
  return(result)
}

.besselap = function(n){
  z = c()
  k = 1
  if(n == 0){
    p = c()
  }else if(n == 1){
    p = c(-1)
  }else if(n == 2){
    p = c(complex(real = -0.8660254037844386467637229, imaginary = 0.4999999999999999999999996),
          complex(real = -0.8660254037844386467637229, imaginary = -0.4999999999999999999999996))
  }else if(n == 3){
    p = c(-0.9416000265332067855971980,
          complex(real = -0.7456403858480766441810907, imaginary = -0.7113666249728352680992154),
          complex(real = -0.7456403858480766441810907, imaginary = 0.7113666249728352680992154))
  }else if(n == 4){
    p = c(complex(real = -0.6572111716718829545787781, imaginary = -0.8301614350048733772399715),
          complex(real = -0.6572111716718829545787781, imaginary = 0.8301614350048733772399715),
          complex(real = -0.9047587967882449459642637, imaginary = -0.2709187330038746636700923),
          complex(real = -0.9047587967882449459642637, imaginary = 0.2709187330038746636700923))
  }else if(n == 5){
    p = c(-.9264420773877602247196260,
          complex(real = -0.8515536193688395541722677, imaginary = -0.4427174639443327209850002),
          complex(real = -0.8515536193688395541722677, imaginary = 0.4427174639443327209850002),
          complex(real = -.5905759446119191779319432, imaginary = -0.9072067564574549539291747),
          complex(real = -.5905759446119191779319432, imaginary = 0.9072067564574549539291747))
  }else if(n == 6){
    p = c(
      complex(real = -.9093906830472271808050953, imaginary = -0.1856964396793046769246397),
      complex(real = -.9093906830472271808050953, imaginary = 0.1856964396793046769246397),
      complex(real = -.7996541858328288520243325, imaginary = -0.5621717346937317988594118),
      complex(real = -.7996541858328288520243325, imaginary = 0.5621717346937317988594118),
      complex(real = -.5385526816693109683073792, imaginary = -.9616876881954277199245657),
      complex(real = -.5385526816693109683073792, imaginary = .9616876881954277199245657))
  }else if(n == 7){
    p = c(
      -.9194871556490290014311619,
      complex(real = -.8800029341523374639772340, imaginary = - .3216652762307739398381830),
      complex(real = -.8800029341523374639772340, imaginary = .3216652762307739398381830),
      complex(real = -.7527355434093214462291616, imaginary = - .6504696305522550699212995),
      complex(real = -.7527355434093214462291616, imaginary = .6504696305522550699212995),
      complex(real = -.4966917256672316755024763, imaginary = - 1.002508508454420401230220),
      complex(real = -.4966917256672316755024763, imaginary = 1.002508508454420401230220))
  }else if(n == 8){
    p = c(
      complex(real = -.9096831546652910216327629, imaginary = - .1412437976671422927888150),
      complex(real = -.9096831546652910216327629, imaginary = .1412437976671422927888150),
      complex(real = -.8473250802359334320103023, imaginary = - .4259017538272934994996429),
      complex(real = -.8473250802359334320103023, imaginary = .4259017538272934994996429),
      complex(real = -.7111381808485399250796172, imaginary = -.7186517314108401705762571),
      complex(real= -.7111381808485399250796172, imaginary = .7186517314108401705762571),
      complex(real = -.4621740412532122027072175, imaginary = - 1.034388681126901058116589),
      complex(real = -.4621740412532122027072175, imaginary = 1.034388681126901058116589)
    )
  }
  return(list(z = z,p = p,k = k))
}

.bilinear = function(z, p, k, Fs){
  Fs = 2*Fs
  pd = (1 + p/Fs)/(1 - p/Fs)
  zd = (1 + z/Fs)/(1 - z/Fs)
  kd = k*prod(Fs - z)/prod(Fs - p)
  zd = c(zd, -rep(1, length(pd) - length(zd)))
  return(list(zd = zd, pd = pd, kd = kd))
}

.besself = function(Fs, Fc, order){
  zpkPrototype = .besselap(order)
  zpkPrototype = sftrans(Zpg(zpkPrototype$z, zpkPrototype$p, zpkPrototype$k), W = Fc*2*pi)
  zpkPrototype = .bilinear(zpkPrototype$zero, zpkPrototype$pole, zpkPrototype$gain, Fs)
  armaCoeffs = as.Arma(Zpg(zpkPrototype$z, zpkPrototype$p, zpkPrototype$k))
  return(armaCoeffs)
}
