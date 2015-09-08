#ifndef STUSB_H_
#define STUSB_H_
#define SAMPLE_INTERFACE_NBR 0x00
#define STUSB_NAME_MAX       64
DRIVER_INITIALIZE DriverEntry;
DRIVER_ADD_DEVICE Stusb_PnPAddDevice;
DRIVER_UNLOAD Stusb_Unload;
__drv_dispatchType(IRP_MJ_CREATE)
DRIVER_DISPATCH Stusb_Create;
__drv_dispatchType(IRP_MJ_CLOSE)
DRIVER_DISPATCH Stusb_Close;
__drv_dispatchType(IRP_MJ_PNP)
DRIVER_DISPATCH Stusb_DispatchPnp;
__drv_dispatchType(IRP_MJ_DEVICE_CONTROL)
DRIVER_DISPATCH Stusb_ProcessIOCTL;
__drv_dispatchType(IRP_MJ_POWER)
DRIVER_DISPATCH Stusb_DispatchPower;
typedef struct _DEVICE_EXTENSION
{
	// physical device object
	PDEVICE_OBJECT	PhysicalDeviceObject;        

	// Device object we call when submitting Urbs/Irps to the USB stack
	PDEVICE_OBJECT	StackDeviceObject;		

	// Indicates that we have recieved a STOP message
	BOOLEAN Stopped;

	// Indicates that we are enumerated and configured.  Used to hold
	// of requests until we are ready for them
	BOOLEAN Started;

	// Indicates the device needs to be cleaned up (ie., some configuration
	// has occurred and needs to be torn down).
	BOOLEAN NeedCleanup;

	// configuration handle for the configuration the
	// device is currently in
	USBD_CONFIGURATION_HANDLE ConfigurationHandle;

	// ptr to the USB device descriptor for this device
	PUSB_DEVICE_DESCRIPTOR DeviceDescriptor;

	// we support up to one interface
	PUSBD_INTERFACE_INFORMATION Interface;

	// the number of device handles currently open to the device object.
	// Gets incremented by Create and decremented by Close
	ULONG                OpenHandles;

	// Name buffer for our named Functional device object link
	WCHAR DeviceLinkNameBuffer[STUSB_NAME_MAX];
	
	// This member is used to store the URB status of the
	// most recently failed URB.  If a USB transfer fails, a caller
	// can use IOCTL_EZUSB_GET_LAST_ERROR to retrieve this value.
	// There's only room for one, so you better get it quick (or at
	// least before the next URB failure occurs).
	USBD_STATUS LastFailedUrbStatus;

	// use counter for the device.  Gets incremented when the driver receives
	// a request and gets decremented when a request is completed.
	LONG usage;

	// this ev gets set when it is ok to remove the device
	KEVENT evRemove;

	// TRUE if we're trying to remove this device
	BOOLEAN removing;

	BOOLEAN Configed;

} DEVICE_EXTENSION, *PDEVICE_EXTENSION;

VOID
Stusb_ReleaseDeviceMem(
	IN PDEVICE_OBJECT fdo
	);
VOID
Stusb_AbortAllPipes(
	IN PDEVICE_OBJECT fdo
	);
NTSTATUS
Stusb_CreateDeviceObject(
    IN PDRIVER_OBJECT DriverObject,
    IN PDEVICE_OBJECT *DeviceObject,
    LONG Instance
	);

VOID
Stusb_Unload(
    IN PDRIVER_OBJECT DriverObject
    );
NTSTATUS
Stusb_PnPAddDevice(
    IN PDRIVER_OBJECT DriverObject,
    IN PDEVICE_OBJECT PhysicalDeviceObject
);
NTSTATUS
Stusb_HandleRemoveDevice(
   IN PDEVICE_OBJECT fdo,
   IN PIRP Irp
   );
NTSTATUS
Stusb_RemoveDevice(
    IN  PDEVICE_OBJECT fdo
    );
VOID UnlockDevice(
   PDEVICE_OBJECT fdo
);
BOOLEAN LockDevice(
   IN PDEVICE_OBJECT fdo
   );
NTSTATUS
CompleteRequest(
   IN PIRP Irp,
   IN NTSTATUS status,
   IN ULONG info
   );
NTSTATUS
Stusb_DefaultPnpHandler(
   IN PDEVICE_OBJECT fdo,
   IN PIRP Irp
   );
NTSTATUS
Stusb_Close(
    IN PDEVICE_OBJECT fdo,
    IN PIRP Irp
    );
NTSTATUS
Stusb_Create(
    IN PDEVICE_OBJECT fdo,
    IN PIRP Irp
    );
NTSTATUS
Stusb_DispatchPnp(
   IN PDEVICE_OBJECT fdo,
   IN PIRP           Irp
   );
NTSTATUS
DriverEntry(
	IN PDRIVER_OBJECT DriverObject,
	IN PUNICODE_STRING RegisteryPath
	);
NTSTATUS
ForwardAndWait(
   IN PDEVICE_OBJECT fdo,
   IN PIRP Irp
   );
NTSTATUS 
OnRequestComplete(
   IN PDEVICE_OBJECT fdo,
   IN PIRP Irp,
   IN PKEVENT pev
   );
NTSTATUS
Stusb_HandleStartDevice(
   IN PDEVICE_OBJECT fdo,
   IN PIRP Irp
   );
NTSTATUS
Stusb_StartDevice(
    IN  PDEVICE_OBJECT fdo
    );
NTSTATUS
Stusb_CallUSBD(
    IN PDEVICE_OBJECT fdo,
    IN PURB Urb
    );
NTSTATUS
Stusb_ConfigureDevice(
    IN  PDEVICE_OBJECT fdo
    );
NTSTATUS
Stusb_SelectInterfaces(
    IN PDEVICE_OBJECT fdo,
    IN PUSB_CONFIGURATION_DESCRIPTOR ConfigurationDescriptor,
    IN PUSBD_INTERFACE_INFORMATION Interface
    ); 
NTSTATUS
Stusb_StopDevice(
   IN  PDEVICE_OBJECT fdo
   );
NTSTATUS
Stusb_AbortPipe(
    IN PDEVICE_OBJECT fdo,
    IN USBD_PIPE_HANDLE PipeHandle
    );
NTSTATUS
Stusb_ProcessIOCTL(
    IN PDEVICE_OBJECT fdo,
    IN PIRP Irp
    );
NTSTATUS
Stusb_Read_Write(
   IN  PDEVICE_OBJECT fdo,
   IN  PIRP Irp
   );
NTSTATUS
Stusb_SurpriseRemoveDevice(
    IN  PDEVICE_OBJECT fdo
    );
NTSTATUS
Stusb_DispatchPower(
	IN PDEVICE_OBJECT fdo,
	IN PIRP Irp
	);
typedef struct _BULK_TRANSFER_CONTROL
{
   ULONG pipeNum;
} BULK_TRANSFER_CONTROL, *PBULK_TRANSFER_CONTROL;

#define STUSB_IOCTL_INDEX                 0x0800
#define IOCTL_STUSB_BULK_READ             CTL_CODE(FILE_DEVICE_UNKNOWN,  \
                                                   STUSB_IOCTL_INDEX+19,\
                                                   METHOD_OUT_DIRECT,  \
                                                   FILE_ANY_ACCESS)
#define IOCTL_STUSB_BULK_WRITE            CTL_CODE(FILE_DEVICE_UNKNOWN,  \
                                                   STUSB_IOCTL_INDEX+20,\
                                                   METHOD_IN_DIRECT,  \
                                                   FILE_ANY_ACCESS)
#endif
