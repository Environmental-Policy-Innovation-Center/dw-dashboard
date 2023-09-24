

import boto3

#-----------------------------------------------------------
def main():


    cluster_name_str = 'ep_core__dev_us-east-1'
    service_name_str = 'ep_app__dw_dashboard__dev_us-east-1'

    exists = check_service_exists(cluster_name_str, service_name_str)

    if exists:
        print(f"Service {service_name_str} exists in cluster {cluster_name_str}.")
    else:
        print(f"Service {service_name_str} does not exist in cluster {cluster_name_str}.")

#-----------------------------------------------------------
def check_service_exists(p_cluster_name_str, p_service_name_str):
    
    client = boto3.client('ecs')
    
    try:
        response = client.describe_services(
            cluster=p_cluster_name_str,
            services=[p_service_name_str]
        )
        
        if len(response['services']) > 0:
            service = response['services'][0]
            if service['status'] != 'INACTIVE':
                return True
    except Exception as e:
        print(f"An error occurred: {e}")
    
    return False

#-----------------------------------------------------------
main()