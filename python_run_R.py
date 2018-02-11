import csv
import os
import operator
import statistics
import rpy2.robjects as robjects

def calc_std(stock_close_price):
    return statistics.stdev(stock_close_price)

def calc_mean(stock_std_dict):
    return statistics.mean(list(stock_std_dict.values()))

def ranking_stock_std(stock_std_dict):    
    return sorted(stock_std_dict.items(), key=operator.itemgetter(1))

def open_script(stock_directory, script, stock,script_switch, long_money=10000, short_money=10000):
    data=""
    with open(script+".R") as file:  
        data = file.read() 
    data=data.replace("STOCK_NAME", stock) 
    data=data.replace("STOCK_DIRECTORY", stock_directory)
    data=data.replace("LONG_MONEY", str(long_money))
    data=data.replace("SHORT_MONEY", str(short_money))
    data=data.replace("RUN_WITHOUT_EX", str(script_switch))
    return data

def runR(script):
    return robjects.r(script)

def main():
    #initialize variable
    stock_std_dict = {}
    STOCK_DIRECTORY = '0050/'
    pass_file = ['.DS_Store']
    RUN_SHOW_SORTED_STOCK_STD = False
    #load data 
    for filename in os.listdir(STOCK_DIRECTORY):
        if filename in pass_file:
            continue
        stock_close_price = []
        stock_name = filename.replace(".txt","")
        with open(STOCK_DIRECTORY + filename) as csvfile:
            readCSV = csv.reader(csvfile, delimiter=',')
            for row in readCSV:
                if row[4]!='Close':
                    price = float(row[4])
                    stock_close_price.append(price)
            stock_std_dict[stock_name] = calc_std(stock_close_price)

    #calculate the std mean
    std_mean = calc_mean(stock_std_dict)
    print("mean of stock STD:")
    print(std_mean)

    if RUN_SHOW_SORTED_STOCK_STD:
        #sort the std
        for x in ranking_stock_std(stock_std_dict):
            print(x)
    #initialize variable
    result_dict={}
    win_time=("stock_name",0.,"")
    win_loss=("stock_name",0.,"")
    PF=("stock_name",0.,"")
    profit_without_service_charge=("stock_name",0.,"")
    service_charge=("stock_name",0.,"")
    total_profit=("stock_name",0.,"")
    #start running strategy
    for stock_name,stock_std in stock_std_dict.items():
        script = 'script1'
        strategy_name = 'boolinger bands'
        print('===============')
        print('Running ' + strategy_name + ' with ' + stock_name)
        #get strategy script
        for script_switch in ["TRUE","FALSE"]:
            scripts = open_script(STOCK_DIRECTORY, script, stock_name, script_switch)
            r = runR(scripts)
            print("win_time/win_loss/PF/profit_without_service_charge/service_charge/total_profit")
            result_dict[stock_name]=(r[0].split(","))
            rt = r[0].split(",")
            script_type = "Without EX" if script_switch else "With EX"
            if float(rt[0]) > win_time[1]:
                print("update win_time")
                win_time=(stock_name,float(rt[0]),script_type)
            if float(rt[1]) > win_loss[1]:
                print("update win_loss")
                win_loss=(stock_name,float(rt[1]),script_type)
            if float(rt[2]) > PF[1]:
                print("update PF")
                PF=(stock_name,float(rt[2]),script_type)
            if float(rt[3]) > profit_without_service_charge[1]:
                print("update profit_without_service_charge")
                profit_without_service_charge=(stock_name,float(rt[3]),script_type)
            if float(rt[4]) > service_charge[1]:
                print("update service_charge")
                service_charge=(stock_name,float(rt[4]),script_type)
            if float(rt[5]) > total_profit[1]:
                print("update total_profit")
                total_profit=(stock_name,float(rt[5]),script_type)
            print(r[0])
            print()
    print(result_dict)
    print(win_time)
    print(win_loss)
    print(PF) 
    print(profit_without_service_charge)
    print(service_charge)
    print(total_profit)
if __name__ == '__main__':
    main()