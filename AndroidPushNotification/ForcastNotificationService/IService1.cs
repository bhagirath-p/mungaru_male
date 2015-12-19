using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Serialization;
using System.ServiceModel;
using System.Text;

namespace ForcastNotificationService
{
    [ServiceContract]
    public interface IService1
    {
        [OperationContract]
        void ReceiveDayStatus(string value);
    }
}
