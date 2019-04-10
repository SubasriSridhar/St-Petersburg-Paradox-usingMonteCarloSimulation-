#  This code will uses Monte Carlo simulation to find the average win or loss amount
#  @Pay_To_Play_Array vector contains amounts paid to play the game and @Avg_Number_of_Games_Array vector contains the number of games played
#  @Coin_Sides is the possible outcome from coin toss

Pay_To_Play_Array<-c(1:50) 
Number_of_Rows=length(Pay_To_Play_Array) 
Avg_Number_of_Games_Array<-c(1:50) 
Avg_Number_of_Games=length(Avg_Number_of_Games_Array)
Coin_Sides=c('H','T')

# @Simulation , the number of simulations for each game.
# The game logic will be executed and the average win or loss will be stored in @Net_Profit matrix. The row and column name for the matrix will populate from 'Pay_To_Play_Array' and 'Avg_Number_of_Games_Array' Matrix respectively
Simulation=50000 
Net_Profit = array(0, c(Number_of_Rows,Avg_Number_of_Games)) 
colnames(Net_Profit)<-Avg_Number_of_Games_Array 
rownames(Net_Profit)<-Pay_To_Play_Array 

for(k in 1:Number_of_Rows) # loop k for many bet amounts
{
	for(j in 1:Avg_Number_of_Games)  #  loop j for average number of games
	{
		Pay_To_Play=Pay_To_Play_Array[k] 
		CoinTossSum= rep(0, Simulation) # This vector is intialized to handle wining amount in each simulation
		X = Simulation*Avg_Number_of_Games_Array[j]
		X=1:X
		#  loop i for each game to run @'Simulation' times (Monte Carlo Simulation). @Temp will count the number of toss in each game until the coin turns Head. Then the payoff (2^Temp) will be stored in a vector
		for(i in X) 
		{
			Temp=1
			TossResult='T'
			while (TossResult == 'T')
			{
				TossResult=sample(Coin_Sides,1, replace=TRUE)
				if (TossResult == 'H')
				{
					CoinTossSum[i]=2^Temp
					break
				}
			Temp=Temp+1
			}
		}
		# Output the mean payoff of all games and calculate the average win or loss
		CoinTossAvg = mean(CoinTossSum)
		Net_Profit[k,j]<- CoinTossAvg-Pay_To_Play
	}
}
Net_Profit
data.frame(Net_Profit) 